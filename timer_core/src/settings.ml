open! Fp

type units = Lbs | Kg
type sound_mode = Mix | Over

type t = {
  units : units;
  window_s : int;
  target : float;
  sound : bool;
  countdown_s : float;
  rest_s : float;
  eng_pct : float;
  dis_pct : float;
  tol_pct : float;
  gripper : string;
  side : string;
  render_delay_ms : float;
  pc_auto : bool;
  planned_reps : int;
  keep_awake : bool;
  page_zoom : float;
  sound_mode : sound_mode;
  volume : float;
  extra : (string * Json.t) list;
}

let storage_key = "grip-gains-force.v2"
let kg_to_lbs = 2.20462
let windows = [ 1; 5; 10; 30 ]

let defaults =
  {
    units = Lbs;
    window_s = 5;
    target = 45. /. kg_to_lbs;
    sound = true;
    countdown_s = 20.;
    rest_s = 20.;
    eng_pct = 90.;
    dis_pct = 70.;
    tol_pct = 7.5;
    gripper = "crusher";
    side = "left";
    render_delay_ms = 300.;
    pc_auto = false;
    planned_reps = 4;
    keep_awake = true;
    page_zoom = 85.;
    sound_mode = Mix;
    volume = 100.;
    extra = [];
  }

let known =
  [
    "units";
    "windowS";
    "target";
    "sound";
    "countdownS";
    "restS";
    "engPct";
    "disPct";
    "tolPct";
    "gripper";
    "side";
    "renderDelayMs";
    "pcAuto";
    "plannedReps";
    "keepAwake";
    "pageZoom";
    "soundMode";
    "volume";
  ]

let of_json j =
  let d = defaults in
  let get k f default = match Option.bind (Json.member k j) f with Some v -> v | None -> default in
  let num k default = get k Json.to_number default in
  let str k default = get k Json.to_string_opt default in
  let bool k default = get k Json.to_bool default in
  let window_s =
    let w = num "windowS" (float_of_int d.window_s) in
    match List.find_opt (fun x -> float_of_int x = w) windows with Some x -> x | None -> 5
  in
  let volume = num "volume" d.volume in
  let page_zoom = num "pageZoom" d.page_zoom in
  {
    units = (match Json.member "units" j with Some (Json.String "lbs") -> Lbs | Some (Json.String _) -> Kg | _ -> d.units);
    window_s;
    target = num "target" d.target;
    sound = bool "sound" d.sound;
    countdown_s = num "countdownS" d.countdown_s;
    rest_s = num "restS" d.rest_s;
    eng_pct = num "engPct" d.eng_pct;
    dis_pct = num "disPct" d.dis_pct;
    tol_pct = num "tolPct" d.tol_pct;
    gripper = str "gripper" d.gripper;
    side = str "side" d.side;
    render_delay_ms = num "renderDelayMs" d.render_delay_ms;
    pc_auto = bool "pcAuto" d.pc_auto;
    planned_reps = int_of_float (Float.ceil (num "plannedReps" (float_of_int d.planned_reps)));
    keep_awake = bool "keepAwake" d.keep_awake;
    page_zoom = (if page_zoom >= 50. && page_zoom <= 150. then page_zoom else 85.);
    sound_mode = (match Json.member "soundMode" j with Some (Json.String "over") -> Over | _ -> Mix);
    volume = (if volume >= 10. && volume <= 150. then volume else 100.);
    extra =
      (match j with
      | Json.Object l ->
          (* Later duplicates overwrite earlier ones but keep the first position. *)
          let keys = List.fold_left (fun acc (k, _) -> if List.mem k acc || List.mem k known then acc else k :: acc) [] l in
          List.rev_map (fun k -> (k, Option.get (Json.member k j))) keys
      | _ -> []);
  }

let of_string = function
  | None -> defaults
  | Some s -> ( match Json.parse s with Ok j -> of_json j | Error _ -> defaults)

let to_json s =
  let n f = Json.Number f and b x = Json.Bool x and st x = Json.String x in
  Json.Object
    ([
       ("units", st (match s.units with Lbs -> "lbs" | Kg -> "kg"));
       ("windowS", n (float_of_int s.window_s));
       ("target", n s.target);
       ("sound", b s.sound);
       ("countdownS", n s.countdown_s);
       ("restS", n s.rest_s);
       ("engPct", n s.eng_pct);
       ("disPct", n s.dis_pct);
       ("tolPct", n s.tol_pct);
       ("gripper", st s.gripper);
       ("side", st s.side);
       ("renderDelayMs", n s.render_delay_ms);
       ("pcAuto", b s.pc_auto);
       ("plannedReps", n (float_of_int s.planned_reps));
       ("keepAwake", b s.keep_awake);
       ("pageZoom", n s.page_zoom);
       ("soundMode", st (match s.sound_mode with Mix -> "mix" | Over -> "over"));
       ("volume", n s.volume);
     ]
    @ s.extra)

let to_string s = Json.to_string (to_json s)
let engage_kg s = s.target *. s.eng_pct /. 100.
let fail_kg s = s.target *. s.dis_pct /. 100.
let tolerance_kg s = s.target *. s.tol_pct /. 100.
let to_display s kg = match s.units with Lbs -> kg *. kg_to_lbs | Kg -> kg
let of_display s v = match s.units with Lbs -> v /. kg_to_lbs | Kg -> v
let unit_label s = match s.units with Lbs -> "lbs" | Kg -> "kg"
let format ?(digits = 1) s kg = Jsnum.to_fixed (to_display s kg) digits

type number_field = Countdown_s | Rest_s | Eng_pct | Dis_pct | Tol_pct | Render_delay_ms

let limits = function
  | Countdown_s -> (3., 180.)
  | Rest_s -> (3., 300.)
  | Eng_pct -> (10., 100.)
  | Dis_pct -> (5., 95.)
  | Tol_pct -> (0.5, 25.)
  | Render_delay_ms -> (0., 2000.)

let set_number s field v =
  let lo, hi = limits field in
  let next =
    match field with
    | Countdown_s -> { s with countdown_s = v }
    | Rest_s -> { s with rest_s = v }
    | Eng_pct -> { s with eng_pct = v }
    | Dis_pct -> { s with dis_pct = v }
    | Tol_pct -> { s with tol_pct = v }
    | Render_delay_ms -> { s with render_delay_ms = v }
  in
  if (not (Float.is_finite v)) || v < lo || v > hi || next.dis_pct >= next.eng_pct then None else Some next

let target_step s = match s.units with Lbs -> 1. | Kg -> 0.5

let step_target s delta =
  let st = target_step s in
  let v = Float.max st ((Jsnum.round (to_display s s.target /. st) *. st) +. (float_of_int delta *. st)) in
  { s with target = of_display s v }

let set_target_display s v = if Float.is_finite v && v > 0. then Some { s with target = of_display s v } else None

let set_planned_reps s n =
  { s with planned_reps = int_of_float (Float.min 20. (Float.max 1. (Jsnum.round n))) }

let zoom_min = 50.
let zoom_max = 150.
let zoom_step = 5.
let set_zoom s v = { s with page_zoom = Float.min zoom_max (Float.max zoom_min (Jsnum.round v)) }

let step_zoom s delta =
  if delta < 0 then set_zoom s ((Float.ceil (s.page_zoom /. zoom_step) *. zoom_step) -. zoom_step)
  else set_zoom s ((Float.floor (s.page_zoom /. zoom_step) *. zoom_step) +. zoom_step)

let keep_set_fields ~current s =
  {
    s with
    units = current.units;
    target = current.target;
    gripper = current.gripper;
    side = current.side;
    planned_reps = current.planned_reps;
    countdown_s = current.countdown_s;
    rest_s = current.rest_s;
    eng_pct = current.eng_pct;
    dis_pct = current.dis_pct;
    tol_pct = current.tol_pct;
  }
