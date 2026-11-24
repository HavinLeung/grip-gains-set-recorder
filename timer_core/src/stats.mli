(** Summary statistics, as in the MVP (from [StatisticsUtilities.swift]).

    All functions return [0.] for empty input, as the JS does, and sum left
    to right from [0.] so results are bit-identical to [Array.reduce]. *)

val mean : float array -> float
val median : float array -> float
(** Middle value, or the mean of the two middle values. *)

val sd : float array -> float
(** Sample standard deviation ([n − 1] denominator); [0.] below 2 values. *)

val pop_sd : float array -> float
(** Population standard deviation ([n] denominator). *)

val percentile_sorted : float array -> float -> float
(** [percentile_sorted sorted p] for [p ∈ [0, 1]]: linear interpolation
    between closest ranks, [s.(lo) + (i − lo)·(s.(hi) − s.(lo))] with
    [i = p·(n − 1)]. The input must be sorted ascending. *)

val trimmed_median : ?fraction:float -> float array -> float
(** Median after dropping [floor(n · fraction)] values (default 0.3) from
    each end {e in arrival order} (not sorted order: the pickup and release
    of a weighed load sit at the ends). Plain median below 5 values. *)

val sorted : float array -> float array
(** An ascending copy ([[...a].sort((x, y) => x - y)]). *)

(** Welford's running mean and sample SD, for the live readout during a
    rep. Immutable: [push] returns a new value. *)
module Running : sig
  type t = private { n : int; m : float; q : float }

  val empty : t
  val push : t -> float -> t
  val mean : t -> float
  val sd : t -> float
  (** [0.] below 2 values. *)
end
