(** What the MVP plays for each {!Session.event} (tones from
    [CountdownSound.swift]); the UI synthesises them (WebAudio sine with a
    10 ms attack and 30 ms release, peak {!peak_gain}). *)

type tone = { freq : float;  (** Hz *) duration : float;  (** s *) delay_ms : float  (** after the event *) }

val tones : Settings.t -> Session.event -> tone list
(** Empty when sound is off or the event is silent:

    - countdown 5, 4, 3: 880 Hz 0.12 s; 2: 660 Hz 0.18 s; 1: 1174.66 Hz 0.24 s;
    - GO ([Rep_started]): 1320 Hz 0.30 s;
    - rest ([Rest_started]): 440 Hz 0.3 s;
    - set complete: 660 Hz 0.15 s, then 1320 Hz 0.3 s 180 ms later;
    - off target: 1320 Hz (too heavy) or 440 Hz (too light), 0.15 s. *)

val peak_gain : Settings.t -> float
(** [0.35 · volume / 100]. *)

val test_tones : tone list
(** The "Test sound" button: 880, 1320 (+250 ms), 440 (+500 ms). *)

val volume_preview : tone
(** Played after the volume slider changes: 880 Hz 0.15 s. *)
