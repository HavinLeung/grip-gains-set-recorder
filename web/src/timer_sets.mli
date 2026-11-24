(** The sets recorded in this browser: every set is written to the device
    ({!Timer_db}) as it is recorded and stays there until it is deleted. The
    life of a set is described at the top of the implementation. *)

open Timer_core

type t

type callbacks = {
  on_change : unit -> unit;  (** a record changed: re-render *)
  on_store_error : unit -> unit;  (** a write to the device storage failed *)
  log : string -> unit;  (** a line for the diagnostic log *)
}

val create : callbacks -> t

val start : t -> unit Js.Promise.t
(** Opens the device storage and reads what it holds; sets a closed page left
    unreviewed are kept as they were recorded. *)

(** {2 What is on this device} *)

val records : t -> Timer_db.record list
(** Oldest first. *)

val find : t -> string -> Timer_db.record option
(** By set id; constant time. *)

(** {2 The life of a set} *)

val save_recording : t -> manual:bool -> History.set -> bool Js.Promise.t
(** After each rep: keep what has been recorded so far. [false]: the device
    refused it. *)

val confirm : t -> manual:bool -> History.set -> bool Js.Promise.t
(** The review's Save: the set as corrected. [false]: the device refused to
    store it, so it is kept for this visit only. *)

val discard : t -> History.set -> bool Js.Promise.t
(** The review's Discard. *)

val delete : t -> string -> unit Js.Promise.t

(** {2 Traces} *)

val trace : t -> string -> History.set option Js.Promise.t
(** The set as recorded (rep graphs included). A manual set, which has no trace,
    is rebuilt from its record. *)

val update_trace : t -> string -> History.set -> bool Js.Promise.t
(** Stores the set again after its PowerCurve upload state changed. *)
