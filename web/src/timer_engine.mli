(** The Timer's controller: the I/O shell around {!Timer_core.Session}.

    It owns the session state, the graph's force buffer, the Bluetooth
    connection, audio, the wake lock, the diagnostic log and the set storage
    ({!Timer_sets}). It feeds the core (every Bluetooth notification, a tick
    every 100 ms, the user's actions) and acts on the events the core returns.

    There is one engine for the page, outside React. A view {!subscribe}s for
    structural changes (connection, phase, reps, ...) and reads the state
    through the functions below; what moves every frame (force, timer, graph) it
    draws from the callback it gives {!attach_view}. *)

open Timer_core

type t

(** {1 Lifetime} *)

val get : unit -> t
(** The page's engine, made on first use. *)

val subscribe : t -> (unit -> unit) -> unit -> unit
(** [subscribe t f] calls [f] whenever something a view shows has changed; the
    result unsubscribes. *)

val attach_view : t -> (t -> render_t:float -> now:float -> unit) -> unit
(** Starts the frame loop: the callback runs every animation frame with the page
    time to draw the graph at ([render_t], s) and [performance.now()]. *)

val detach_view : t -> unit

(** {1 Settings} *)

val settings : t -> Settings.t

val change : t -> Settings.t -> unit
(** Stores and applies new settings. During a set, everything that shapes the
    set (see {!Timer_core.Settings.keep_set_fields}, and manual mode) stays as
    it is. *)

val manual : t -> bool
(** Manual mode is on ({!Timer_prefs}). *)

val set_manual : t -> bool -> unit
(** Ignored during a set. Turning it on disconnects a connected Progressor. *)

val set_planned_reps : t -> float -> unit
val pick_hand : t -> gripper:string -> side:string -> unit

val set_target : t -> float -> unit
(** The target, in kg. *)

(** {1 The Progressor} *)

type conn = Off | Connecting | Live | Reconnecting

val conn : t -> conn

val streaming : t -> bool
(** Connected and measuring. *)

val conn_step : t -> string
(** The connection step in progress, for the readout. *)

val battery : t -> int option
(** Percent. *)

val hz : t -> int
(** Samples per second over the last second ([0] when not streaming). *)

val connect : t -> accept_all:bool -> unit
(** Opens the browser's device list. [accept_all]: list every Bluetooth device.
*)

val disconnect : t -> unit Js.Promise.t
(** Ends a set in progress. *)

val tare : t -> unit
val power_off : t -> unit

(** {1 The set} *)

val session : t -> Session.t
val buffer : t -> Force_buffer.t
val in_set : t -> bool
val start_set : t -> unit
val end_set : t -> unit

val end_rep : t -> unit
(** Ends the current rep now and keeps it. In manual mode a tap in a rep's first
    second is ignored (the second tap of a double tap). *)

val set_is_manual : t -> bool
(** The set in progress (or in review) was started in manual mode. *)

val last_set : t -> History.set option
(** The set that ended last (the idle timer shows its last rep). *)

(** {2 The end-of-set review} *)

type review = {
  set : History.set;
  manual : bool;  (** a manual-mode set: hold times only *)
  kg : float;  (** the weight to save, as corrected so far *)
  discard_armed : bool;  (** Discard was pressed once; again discards *)
}

val review : t -> review option
val review_step : t -> float
val review_set_kg : t -> float -> unit
val review_nudge : t -> int -> unit
val review_save : t -> unit
val review_discard : t -> unit

(** {1 History} *)

val sets : t -> Timer_sets.t
val gripper_name : string -> string

type sheet = {
  sheet_id : string;
  trace : History.set option;
  loading : bool;
  delete_armed : bool;
}
(** An open set sheet: the set's id and, once loaded, the set as recorded. *)

val sheet : t -> sheet option
val open_sheet : t -> string -> unit
val close_sheet : t -> unit

val sheet_delete : t -> unit
(** The sheet's Delete: the first press arms it, the second deletes. *)

val delete_set : t -> string -> unit

(** {2 PowerCurve} *)

val pc_token : t -> string
val pc_token_error : t -> bool
val pc_valid : t -> bool
val set_pc_token : t -> string -> unit
val pc_in_flight : t -> string -> bool
val pc_state : t -> string -> History.set -> History.pc option
val upload_pc : t -> string -> History.set -> unit

(** {1 Messages, sound and the log} *)

type notice = { lead : string; code : string option; lines : string list }
(** The red box under the connect button: a lead sentence, an optional error
    shown as code, and extra lines. *)

val error : t -> notice option
val current_toast : t -> string option
val toast : t -> string -> unit

val unlock_audio : t -> unit
(** iOS only lets audio start from a tap: call from the tap that should be
    heard. *)

val play : t -> Sound.tone list -> unit
val diagnostics : t -> Timer_log.t
val log : t -> string -> unit
