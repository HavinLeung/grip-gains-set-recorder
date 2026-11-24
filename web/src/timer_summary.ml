(* A saved set's summary: the small JSON object kept in its record (Timer_db), which the
   history list, the set sheet and "Copy JSON" read without loading the set's force traces.
   Pure, so the checks (timer_checks.ml) cover every conversion.

   Units: the timer core works in kg (Timer_core.Settings stores everything in kg); the app
   shows lbs (Timer_engine keeps Settings.units at Lbs) and summaries are in lbs. The one
   conversion factor is Timer_core.Settings.kg_to_lbs (2.20462). *)

open Timer_core

let lbs_of_kg kg = kg *. Settings.kg_to_lbs
let kg_of_lbs lbs = lbs /. Settings.kg_to_lbs

(* Two decimals, as the per-rep stats are stored. *)
let r2 = Jsnum.r2

(* A set's own id is [Date.now().toString(36)] at its start. *)
let ms_of_base36 s =
  let n = ref 0. and ok = ref (String.length s > 0) in
  String.iter
    (fun c ->
      let d =
        match c with
        | '0' .. '9' -> Char.code c - 48
        | 'a' .. 'z' -> Char.code c - 87
        | _ ->
            ok := false;
            0
      in
      n := (!n *. 36.) +. float_of_int d)
    s;
  if !ok then Some !n else None

external parse_date : string -> float = "parse" [@@mel.scope "Date"]

(* When the set started, ms since the epoch: from its id when that is a plausible base-36 time
   (2000-2100), else from its date_time. *)
let performed_at (set : History.set) =
  match ms_of_base36 set.id with
  | Some ms when ms >= 946684800000. && ms < 4102444800000. -> ms
  | _ ->
      let ms = parse_date set.date_time in
      if Float.is_nan ms then 0. else ms

(* Every hold is > 0 s: a rep the core timed at 0 s (it ended on its first sample) is kept as
   the shortest representable hold rather than dropped. *)
let hold_seconds (r : Rep.t) = Float.max 0.01 r.seconds

let rep1_hold (set : History.set) =
  match set.reps with r :: _ -> Some (hold_seconds r) | [] -> None

(* The type the set turned out to be, from its rep-1 hold. *)
let set_type (set : History.set) =
  Option.bind (rep1_hold set) Set_types.classify

let clamp lo hi x = Float.min hi (Float.max lo x)

let rep_stat (r : Rep.t) =
  let force kg = clamp 0. 2000. (r2 (lbs_of_kg kg)) in
  Js.Json.object_
    (Js.Dict.fromList
       [
         ("median_lbs", Js.Json.number (force r.median_kg));
         ("mean_lbs", Js.Json.number (force r.mean_kg));
         ("sd_lbs", Js.Json.number (force r.sd_kg));
       ])

(* The id a set is stored under: its session UUID (the PowerCurve source_session_id too). *)
let id_of_set (set : History.set) =
  match set.source_session_id with Some u -> u | None -> "timer-" ^ set.id

(* The summary of a set with at least one rep. [weight_lbs] is the export weight (lbs to 0.1,
   History.weight_lbs) so the history, the PowerCurve upload and "Copy JSON" agree.

   [manual]: recorded in manual mode (no Progressor: every rep ended by hand), so the set has
   hold times and nothing else. The core still makes a Rep.t per rep (with no samples: zero
   statistics, an empty graph); those zeros are never stored: the summary says [manual: true]
   and has no rep_stats. *)
let of_set ~manual (set : History.set) : Js.Json.t =
  let num = Js.Json.number in
  Js.Json.object_
    (Js.Dict.fromList
       [
         ("id", Js.Json.string (id_of_set set));
         ("gripper", Js.Json.string set.gripper);
         ("side", Js.Json.string set.side);
         ("performed_at", num (performed_at set));
         ("weight_lbs", num (History.weight_lbs set));
         ("planned_reps", num (float_of_int (max 1 (min 50 set.planned_reps))));
         ("rest_s", num (clamp 0. 3600. set.rest_s));
         ( "rep_seconds",
           Js.Json.array
             (Array.of_list (List.map (fun r -> num (hold_seconds r)) set.reps))
         );
         ( "rep_stats",
           (* a manual set has no force data: absent (null), not zeros *)
           if manual then Js.Json.null
           else Js.Json.array (Array.of_list (List.map rep_stat set.reps)) );
         ( "set_type",
           match set_type set with
           | Some t -> Js.Json.string (Set_types.id t)
           | None -> Js.Json.null );
         ("manual", Js.Json.boolean manual);
       ])

(* --- reading a summary back --- *)

type t = {
  id : string;
  gripper : string;
  side : string;
  performed_at : float;
  weight_lbs : float;
  planned_reps : int;
  rest_s : float;
  rep_seconds : float array;
  manual : bool;
      (** recorded in manual mode: hold times only, no force statistics *)
}

let of_json json =
  let ( let* ) = Option.bind in
  let* id = Decode.string_field "id" json in
  let* gripper = Decode.string_field "gripper" json in
  let* side = Decode.string_field "side" json in
  let* performed_at = Decode.number_field "performed_at" json in
  let* weight_lbs = Decode.number_field "weight_lbs" json in
  let* planned_reps = Decode.int_field "planned_reps" json in
  let* rest_s = Decode.number_field "rest_s" json in
  let* rep_seconds =
    Decode.field "rep_seconds" (Decode.array Js.Json.decodeNumber) json
  in
  let manual = Option.value ~default:false (Decode.bool_field "manual" json) in
  Some
    {
      id;
      gripper;
      side;
      performed_at;
      weight_lbs;
      planned_reps;
      rest_s;
      rep_seconds;
      manual;
    }

(* A manual set as History.set again, from its summary (there is no trace): what the
   PowerCurve upload and the set sheet work with. Its reps have hold times only (the zero
   statistics are never shown or sent). [pc]: the upload state kept with it. *)
let manual_set (s : t) ~pc : History.set =
  let target_kg = kg_of_lbs s.weight_lbs in
  {
    id = Jsnum.to_base36 s.performed_at;
    source_session_id = Some s.id;
    date_time = Jsnum.iso_utc s.performed_at;
    gripper = s.gripper;
    side = s.side;
    target_kg;
    planned_reps = s.planned_reps;
    countdown_s = 0.;
    rest_s = s.rest_s;
    rep_end_after_pct = 0.;
    rep_end_below_pct = 0.;
    tolerance_pct = 0.;
    reps =
      Array.to_list s.rep_seconds
      |> List.map (fun d -> Rep.make ~duration:d [||] ~target_kg);
    pc;
    extra = [];
  }

(* The PowerCurve state of a manual set, kept in its record (as text) since it has no trace to
   hold it: History's own encoding, so it reads back exactly as a trace's would. *)
let pc_to_string (pc : History.pc) = Json.to_string (History.pc_to_json pc)

let pc_of_string text : History.pc option =
  match Json.parse text with
  | Ok j -> Some (History.pc_of_json j)
  | Error _ -> None

(* "Copy JSON": {date_time, gripper, side, weight_lbs, reps: whole seconds}. [gripper] is the
   gripper's lower-cased name ("crusher", "micro"). One set copies as an object; several as an
   array. *)
let export_obj (s : t) =
  Json.Object
    [
      ("date_time", Json.String (Jsnum.iso_utc s.performed_at));
      ("gripper", Json.String (String.lowercase_ascii (Grippers.name s.gripper)));
      ("side", Json.String s.side);
      ( "weight_lbs",
        Json.Number (float_of_string (Jsnum.to_fixed s.weight_lbs 1)) );
      ( "reps",
        Json.Array
          (Array.to_list s.rep_seconds
          |> List.map (fun x -> Json.Number (Float.floor x))) );
    ]

let export_json = function
  | [ one ] -> Json.to_string ~indent:2 one
  | l -> Json.to_string ~indent:2 (Json.Array l)
