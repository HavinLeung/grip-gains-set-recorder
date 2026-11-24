(** User settings, with the MVP's defaults and its localStorage format
    (key {!storage_key}, a JSON object).

    {2 Reading saved settings}

    The MVP does [S = { ...DEFAULTS, ...JSON.parse(saved) }] and then fixes a
    few fields: a [windowS] other than 1, 5, 10 or 30 becomes 5; a
    [soundMode] other than ["over"] becomes ["mix"]; a [volume] outside
    [10, 150] becomes 100; and (in [applyZoom], at start-up) a [pageZoom]
    outside [50, 150] becomes 85. {!of_json} does the same. Differences, all
    for values the MVP's own UI never writes:

    - a known key holding the wrong JSON type (e.g. ["target": "20"]) falls
      back to the default, where the MVP would keep the odd value;
    - [units] other than ["lbs"] reads as [Kg] (the MVP treats every
      non-["lbs"] value as kg, but would write the odd value back);
    - a non-integer [plannedReps] is rounded up (the MVP compares
      [reps.length >= plannedReps], so 4.5 behaves as 5);
    - unparsable JSON or a non-object gives the defaults (as the MVP), and
      an array's indices are not kept as keys (the MVP's spread would).

    Unknown keys are kept in [extra] and written back after the known ones,
    as the spread does. *)

type units = Lbs | Kg
type sound_mode = Mix | Over

type t = {
  units : units;  (** ["units"], default lbs. Display only; everything is stored in kg. *)
  window_s : int;  (** ["windowS"]: graph window, one of {!windows}; default 5. *)
  target : float;  (** ["target"]: kg, default 45 lbs (= 45 / 2.20462 kg). *)
  sound : bool;  (** ["sound"], default true. *)
  countdown_s : float;  (** ["countdownS"], default 20. *)
  rest_s : float;  (** ["restS"], default 20. *)
  eng_pct : float;  (** ["engPct"]: a rep can end once force reaches this % of target; default 90. *)
  dis_pct : float;  (** ["disPct"]: …and then drops below this %; default 70. *)
  tol_pct : float;  (** ["tolPct"]: on-target band ±%; default 7.5. *)
  gripper : string;  (** ["gripper"]: ["crusher"] (default) or ["micro"]. *)
  side : string;  (** ["side"]: ["left"] (default) or ["right"]. *)
  render_delay_ms : float;  (** ["renderDelayMs"], default 300. *)
  pc_auto : bool;  (** ["pcAuto"]: auto-upload to PowerCurve, default false. *)
  planned_reps : int;  (** ["plannedReps"], default 4. *)
  keep_awake : bool;  (** ["keepAwake"], default true. *)
  page_zoom : float;  (** ["pageZoom"] (%), default 85. *)
  sound_mode : sound_mode;  (** ["soundMode"]: ["mix"] (default) or ["over"]. *)
  volume : float;  (** ["volume"] (%), default 100. *)
  extra : (string * Json.t) list;  (** Unknown keys, kept in order. *)
}

val keep_set_fields : current:t -> t -> t
(** [keep_set_fields ~current s] is [s] with everything that shapes a set
    taken from [current]: units, target, gripper, side, planned reps,
    countdown, rest, and the engage, disengage and tolerance percentages.
    What remains (sounds, volume, page size, graph window and delay, keep
    awake, PowerCurve) may change while a set is in progress. *)

val storage_key : string
(** ["grip-gains-force.v2"]. *)

val defaults : t
val kg_to_lbs : float
(** 2.20462. *)

val windows : int list
(** [1; 5; 10; 30] seconds. *)

val of_json : Json.t -> t
val of_string : string option -> t
(** From [localStorage.getItem(storage_key)] ([None] for a missing key). *)

val to_json : t -> Json.t
(** Known keys in the MVP's [DEFAULTS] order, then [extra]; {!Json.to_string}
    of it equals the MVP's [JSON.stringify(S)]. *)

val to_string : t -> string

(** {2 Thresholds} (kg, percentages of the {e current} target) *)

val engage_kg : t -> float
(** [target · eng_pct / 100]. *)

val fail_kg : t -> float
(** [target · dis_pct / 100]. *)

val tolerance_kg : t -> float
(** [target · tol_pct / 100]. *)

(** {2 Display units} *)

val to_display : t -> float -> float
(** kg → the display unit. *)

val of_display : t -> float -> float
val unit_label : t -> string
(** ["lbs"] or ["kg"]. *)

val format : ?digits:int -> t -> float -> string
(** [fmt(kg, d)]: kg in display units, [toFixed(digits)] (default 1). *)

(** {2 Edits the settings controls make} *)

type number_field = Countdown_s | Rest_s | Eng_pct | Dis_pct | Tol_pct | Render_delay_ms

val limits : number_field -> float * float
(** countdown 3–180, rest 3–300, engage 10–100, disengage 5–95,
    tolerance 0.5–25, graph delay 0–2000. *)

val set_number : t -> number_field -> float -> t option
(** The settings inputs' validation: [None] (reject, re-sync the control)
    for a non-finite value, one outside {!limits}, or one that would leave
    [dis_pct ≥ eng_pct]. *)

val target_step : t -> float
(** 1 in lbs, 0.5 in kg (display units). *)

val step_target : t -> int -> t
(** The −/+ buttons: round the displayed target to the step, add
    [delta · step], keep at least one step. *)

val set_target_display : t -> float -> t option
(** Typed target (display units): accepted when finite and [> 0]. *)

val set_planned_reps : t -> float -> t
(** Rounded and clamped to 1–20. *)

val zoom_min : float
val zoom_max : float
val zoom_step : float
val set_zoom : t -> float -> t
(** Rounded and clamped to 50–150. *)

val step_zoom : t -> int -> t
(** The −/+ buttons: to the next multiple of 5 down / up. *)
