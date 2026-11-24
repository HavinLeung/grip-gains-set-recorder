open! Fp

type performance = { rank : float option; prior_session_count : int }

type pc =
  | Uploading
  | Uploaded of { at : string; result : string; performance : performance option }
  | Failed of { at : string; message : string }
  | Unknown_pc of Json.t

type set = {
  id : string;
  source_session_id : string option;
  date_time : string;
  gripper : string;
  side : string;
  target_kg : float;
  planned_reps : int;
  countdown_s : float;
  rest_s : float;
  rep_end_after_pct : float;
  rep_end_below_pct : float;
  tolerance_pct : float;
  reps : Rep.t list;
  pc : pc option;
  extra : (string * Json.t) list;
}

let storage_key = "grip-gains-force.history.v1"
let n f = Json.Number f
let s x = Json.String x

let perf_to_json = function
  | None -> Json.Null
  | Some p ->
      Json.Object
        [
          ("rank", match p.rank with Some r -> n r | None -> Json.Null);
          ("prior_session_count", n (float_of_int p.prior_session_count));
        ]

let pc_to_json = function
  | Uploading -> Json.Object [ ("state", s "uploading") ]
  | Uploaded { at; result; performance } ->
      Json.Object [ ("state", s "uploaded"); ("at", s at); ("result", s result); ("performance", perf_to_json performance) ]
  | Failed { at; message } -> Json.Object [ ("state", s "failed"); ("at", s at); ("message", s message) ]
  | Unknown_pc j -> j

let keys_are j ks = match j with Json.Object l -> List.map fst l = ks | _ -> false
let str k j = Option.bind (Json.member k j) Json.to_string_opt

let pc_of_json j =
  match (str "state" j, str "at" j) with
  | Some "uploading", _ when keys_are j [ "state" ] -> Uploading
  | Some "failed", Some at when keys_are j [ "state"; "at"; "message" ] -> (
      match str "message" j with Some message -> Failed { at; message } | None -> Unknown_pc j)
  | Some "uploaded", Some at when keys_are j [ "state"; "at"; "result"; "performance" ] -> (
      let perf =
        match Json.member "performance" j with
        | Some Json.Null -> Some None
        | Some (Json.Object _ as p) when keys_are p [ "rank"; "prior_session_count" ] -> (
            match (Json.member "rank" p, Json.member "prior_session_count" p) with
            | Some (Json.Number r), Some (Json.Number c) when Float.is_integer c ->
                Some (Some { rank = Some r; prior_session_count = int_of_float c })
            | Some Json.Null, Some (Json.Number c) when Float.is_integer c ->
                Some (Some { rank = None; prior_session_count = int_of_float c })
            | _ -> None)
        | _ -> None
      in
      match (str "result" j, perf) with
      | Some result, Some performance -> Uploaded { at; result; performance }
      | _ -> Unknown_pc j)
  | _ -> Unknown_pc j

let set_to_json st =
  Json.Object
    ([ ("id", s st.id) ]
    @ (match st.source_session_id with Some u -> [ ("source_session_id", s u) ] | None -> [])
    @ [
        ("date_time", s st.date_time);
        ("gripper", s st.gripper);
        ("side", s st.side);
        ("target_kg", n st.target_kg);
        ("planned_reps", n (float_of_int st.planned_reps));
        ("countdown_s", n st.countdown_s);
        ("rest_s", n st.rest_s);
        ("rep_end_after_pct", n st.rep_end_after_pct);
        ("rep_end_below_pct", n st.rep_end_below_pct);
        ("tolerance_pct", n st.tolerance_pct);
        ("reps", Json.Array (List.map Rep.to_json st.reps));
      ]
    @ (match st.pc with Some p -> [ ("pc", pc_to_json p) ] | None -> [])
    @ st.extra)

let known =
  [
    "id";
    "source_session_id";
    "date_time";
    "gripper";
    "side";
    "target_kg";
    "planned_reps";
    "countdown_s";
    "rest_s";
    "rep_end_after_pct";
    "rep_end_below_pct";
    "tolerance_pct";
    "reps";
    "pc";
  ]

let set_of_json j =
  match (j, str "id" j) with
  | Json.Object fields, Some id ->
      let num k = Option.value ~default:0. (Option.bind (Json.member k j) Json.to_number) in
      let reps =
        match Option.bind (Json.member "reps" j) Json.to_list with
        | Some l -> List.filter_map (fun r -> Result.to_option (Rep.of_json r)) l
        | None -> []
      in
      Ok
        {
          id;
          source_session_id = str "source_session_id" j;
          date_time = Option.value ~default:"" (str "date_time" j);
          gripper = Option.value ~default:"" (str "gripper" j);
          side = Option.value ~default:"" (str "side" j);
          target_kg = num "target_kg";
          planned_reps = int_of_float (Float.ceil (num "planned_reps"));
          countdown_s = num "countdown_s";
          rest_s = num "rest_s";
          rep_end_after_pct = num "rep_end_after_pct";
          rep_end_below_pct = num "rep_end_below_pct";
          tolerance_pct = num "tolerance_pct";
          reps;
          pc = (match Json.member "pc" j with None -> None | Some p -> Some (pc_of_json p));
          extra = List.filter (fun (k, _) -> not (List.mem k known)) fields;
        }
  | Json.Object _, None -> Error "set without an id"
  | _ -> Error "set is not an object"

let of_json = function
  | Json.Array l -> List.filter_map (fun j -> Result.to_option (set_of_json j)) l
  | _ -> []

let of_string = function
  | None -> []
  | Some str -> ( match Json.parse str with Ok j -> of_json j | Error _ -> [])

let to_json l = Json.Array (List.map set_to_json l)
let to_string l = Json.to_string (to_json l)

let upsert l st =
  if List.exists (fun x -> x.id = st.id) l then List.map (fun x -> if x.id = st.id then st else x) l else l @ [ st ]

let remove l ids = List.filter (fun x -> not (List.mem x.id ids)) l

let relog st kg =
  if Float.abs (kg -. st.target_kg) > 1e-6 then
    {
      st with
      target_kg = kg;
      reps = List.map (fun (r : Rep.t) -> { r with dev_kg = Jsnum.r2 (r.median_kg -. kg) }) st.reps;
    }
  else st

let weight_lbs st = float_of_string (Jsnum.to_fixed (st.target_kg *. Settings.kg_to_lbs) 1)
let rep_seconds (r : Rep.t) = int_of_float (Float.floor r.seconds)

let export_obj st =
  Json.Object
    [
      ("date_time", s st.date_time);
      ("gripper", s st.gripper);
      ("side", s st.side);
      ("weight_lbs", n (weight_lbs st));
      ("reps", Json.Array (List.map (fun r -> n (float_of_int (rep_seconds r))) st.reps));
    ]

let export_json = function
  | [ one ] -> Json.to_string ~indent:2 (export_obj one)
  | l -> Json.to_string ~indent:2 (Json.Array (List.map export_obj l))

let pc_url = "https://powercurve.tantaluspath.com/api/integrations/grip-gains/sessions"
let pc_helper_version = "3"
let pc_token_key = "grip-gains-force.powercurve-token"

let valid_pc_token t =
  String.length t = 69
  && String.sub t 0 5 = "pcgg_"
  &&
  let ok = ref true in
  for i = 5 to 68 do
    match t.[i] with '0' .. '9' | 'a' .. 'f' -> () | _ -> ok := false
  done;
  !ok

let with_session_id st u = match st.source_session_id with Some _ -> st | None -> { st with source_session_id = Some u }

let pc_payload st ~session_id =
  Json.Object
    [
      ("source_session_id", s (Option.value ~default:session_id st.source_session_id));
      ("date_time", s st.date_time);
      ("gripper", s st.gripper);
      ("side", s st.side);
      ("weight_lbs", n (weight_lbs st));
      ("reps", Json.Array (List.map (fun r -> n (float_of_int (max 1 (rep_seconds r)))) st.reps));
      ("excluded_from_metrics", Json.Bool false);
    ]

let is_safe_integer f = Float.is_integer f && Float.abs f <= 9007199254740991.

let pc_result ~status ~body ~at =
  match status with
  | None -> Failed { at; message = "Couldn't reach PowerCurve. Check the connection and retry." }
  | Some 401 ->
      Failed { at; message = "PowerCurve rejected the token. Get a new one from PowerCurve and update it in Settings." }
  | Some code when code < 200 || code > 299 ->
      let message =
        match str "error" body with
        | Some e when e <> "" -> e
        | _ -> "PowerCurve couldn't save this set (error " ^ string_of_int code ^ ")."
      in
      Failed { at; message }
  | Some _ -> (
      match str "status" body with
      | Some (("created" | "already_uploaded" | "merged") as result) ->
          let performance =
            match Json.member "performance" body with
            | Some (Json.Object _ as p) -> (
                match Option.bind (Json.member "prior_session_count" p) Json.to_number with
                | Some c when is_safe_integer c ->
                    Some
                      {
                        rank = Option.bind (Json.member "rank" p) Json.to_number;
                        prior_session_count = int_of_float c;
                      }
                | _ -> None)
            | _ -> None
          in
          Uploaded { at; result; performance }
      | _ -> Failed { at; message = "PowerCurve sent a reply this page doesn't recognise." })

let interrupted_message = "The upload was interrupted. Retry it."

let pc_shown pc ~in_flight =
  match pc with Some Uploading when not in_flight -> Some (Failed { at = ""; message = interrupted_message }) | pc -> pc

let rank_text perf st =
  match perf with
  | None -> ""
  | Some p ->
      let label = Jsnum.capitalize st.gripper ^ " " ^ Jsnum.capitalize st.side and c = p.prior_session_count in
      if c < 5 then
        "No rank yet: " ^ string_of_int c ^ " earlier " ^ label ^ " set" ^ (if c = 1 then "" else "s") ^ " (a rank needs 5)."
      else (
        match p.rank with
        | None -> "Rank unavailable (" ^ string_of_int c ^ " earlier " ^ label ^ " sets)."
        | Some r ->
            let word =
              if r >= 1. then "Best performance"
              else if r >= 0.75 then "Excellent"
              else if r >= 0.5 then "Solid"
              else if r >= 0.25 then "Room to build"
              else "Below your usual"
            in
            "Rank "
            ^ Jsnum.to_fixed (Float.min 1. (Float.max 0. r)) 2
            ^ {js| · |js}
            ^ word ^ ", against " ^ string_of_int c ^ " earlier " ^ label ^ " sets.")
