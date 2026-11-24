(** The settings that apply to the whole page rather than to a set: the page
    size and whether to keep the screen awake.

    They are stored as ["pageZoom"] and ["keepAwake"] in the same settings
    object the Timer keeps its own settings in ({!storage_key}). The Timer
    leaves those two keys to this module: it writes the object through {!stamp}.
*)

type t = {
  page_zoom : float;  (** Percent, 50 to 150 in steps of 5; default 85. *)
  keep_awake : bool;
      (** Hold the screen wake lock while the Progressor is connected; default
          [true]. *)
}

val storage_key : string
(** The localStorage key of the settings object. *)

val get : unit -> t

val set_page_zoom : float -> unit
(** Rounded and clamped; applied to the page at once. *)

val step_page_zoom : int -> unit
(** The −/+ buttons: to the next multiple of 5 down / up. *)

val set_keep_awake : bool -> unit

val subscribe : (unit -> unit) -> unit -> unit
(** [subscribe f] calls [f] after every change; the result unsubscribes. *)

val use : unit -> t
(** A hook: the current values, re-rendering when they change. *)

val apply : unit -> unit
(** Scales the page to the stored page size (at start; changes apply
    themselves). *)

val stamp : Timer_core.Settings.t -> Timer_core.Settings.t
(** The Timer's settings with this module's two values in place, for writing the
    shared object: whatever the Timer holds for them is ignored. *)
