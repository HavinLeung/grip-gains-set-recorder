(** One rep: the stable-hold analysis (ported from [RepResult.swift] via the
    MVP's [analyzeRep]) and the record saved in history ([makeRep]). *)

(** {1 Analysis} *)

type percentiles = {
  p1 : float;
  p5 : float;
  p10 : float;
  q1 : float;
  med : float;
  q3 : float;
  p90 : float;
  p95 : float;
  p99 : float;
}
(** All rounded with {!Jsnum.r2}. JSON keys: [P1 P5 P10 Q1 Med Q3 P90 P95 P99],
    in that order. *)

type analysis = {
  stable : int * int;
      (** Inclusive sample indices of the stable hold. With [n ≥ 4] samples the
          band is [median ± 3·pop_sd] of the middle half
          ([samples.(n/4) .. samples.(n − n/4 − 1)]); the stable region runs
          from the first to the last sample inside the band. If no sample is
          in the band the whole rep is used. Below 4 samples: [(0, n − 1)]
          ([(0, 0)] when empty). *)
  median : float;  (** r2 of the median of the stable region. *)
  mean : float;  (** r2. *)
  sd : float;  (** Sample SD, rounded to 5 decimals as the JS does ([r2(sd·1000)/1000]). *)
  pct : percentiles;
  dev_kg : float;  (** r2(unrounded median − target). *)
}

val analyze : float array -> target_kg:float -> analysis

(** {1 Saved rep} *)

val store_every : int
(** 4: saved graphs average each run of 4 samples (80 Hz → 20 Hz). *)

type t = {
  seconds : float;  (** r2 of the rep duration. *)
  median_kg : float;
  mean_kg : float;
  sd_kg : float;
  dev_kg : float;
  pct_kg : percentiles;
  stable : int * int;  (** The analysis' stable indices divided by {!store_every} (floored). *)
  graph_hz : float;  (** 80 / {!store_every} = 20 for reps made here. *)
  force_kg : float array;  (** r2 of the mean of each run of 4 samples (the last run may be shorter). *)
  extra : (string * Json.t) list;  (** Unknown keys read from storage, written back unchanged. *)
}

val make : duration:float -> float array -> target_kg:float -> t
(** [makeRep(duration, samples, targetKg)]. *)

val percentile_list : percentiles -> (string * float) list
(** In display/JSON order, with the JSON key names. *)

val to_json : t -> Json.t
(** Keys in the MVP's order: [seconds median_kg mean_kg sd_kg dev_kg pct_kg
    stable graph_hz force_kg], then [extra]. *)

val of_json : Json.t -> (t, string) result
(** Lenient: [seconds] is required; other missing or mistyped numbers read
    as [0.], a missing [graph_hz] as [20.], missing arrays as empty. *)
