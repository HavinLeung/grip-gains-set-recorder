(** The live graph's force history (the MVP's [H]): page times and forces in
    arrival order, plus the shaded grip intervals.

    This is the one mutable structure in the library: it holds up to ten
    minutes at 80 Hz (48 000 samples), too many to copy per sample. The UI
    owns it and feeds it {!Session.event}s with {!apply}.

    Trimming is the MVP's: whenever the length after a push is a multiple
    of 400 and the oldest sample is more than 605 s older than the new one,
    samples older than 600 s are dropped, and so are grips that ended before
    the new oldest sample. *)

type grip = { start : float; mutable stop : float option  (** [None] while the rep runs. *) }
type t

val max_history_s : float
(** 600. *)

val create : unit -> t
val clear : t -> unit
val length : t -> int
val time : t -> int -> float
val force : t -> int -> float
val last_time : t -> float option
val grips : t -> grip list
(** Oldest first. *)

val push : t -> float -> float -> unit
(** [push b t f] ([pushHistory]). *)

val start_grip : t -> float -> unit
val end_grip : t -> float -> unit
(** Closes the newest grip if it is still open. *)

val discard_grip : t -> unit
(** Removes the newest grip if it is still open. *)

val apply : t -> Session.event -> unit
(** [Sample], [Buffer_cleared] and the grip events; others are ignored. *)

val lower_bound : t -> float -> int
(** First index whose time is [≥ x] (binary search; times are assumed
    ascending, as the MVP assumes). *)

val value_at : t -> float -> float
(** Linear interpolation at a page time, holding the first/last value
    outside the data; [0.] when empty. *)
