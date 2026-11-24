(** Set summary statistics (the MVP's [summaryHTML], after
    [SetStatistics.swift]) and the per-rep deviation colouring of the rep
    lists. All in kg; convert with {!Settings.to_display}. *)

type t = {
  reps : int;
  total_seconds : float;  (** Sum of rep seconds. *)
  avg_median_kg : float;  (** Mean of the reps' medians. *)
  avg_sd_kg : float;  (** Mean of the reps' SDs. *)
  dev_kg : float;  (** Mean of the reps' deviations (each rep's own, not mean median − target). *)
  dev_pct : float;  (** [dev_kg / target · 100], [0.] for a zero target. *)
}

val of_set : History.set -> t
(** Means of no reps are [0.]. *)

type deviation = On_target | Heavy | Light

val rep_deviation : History.set -> Rep.t -> float * deviation
(** The rep's deviation as % of the target ([0.] for a zero target), and its
    colour: on target when [|dev_kg|] is below the set's tolerance, else
    heavy or light by sign. *)
