(** The set timer: the MVP's force handler ([P]: idle | rep | weighing), set
    flow ([SET]: off | countdown | rep | rest) and device-clock mapping, as
    one explicit state and a pure {!step}.

    {2 Driving it}

    The UI shell owns one [t] and feeds it {!input}s:

    - every BLE notification as [Packet] (with [performance.now()]);
    - a [Tick] every 100 ms ([setInterval]; the MVP keeps set timing on its
      own timer so countdown beeps keep time when frames slow down);
    - user actions ([Start_set], [End_set] for "End set early", [End_rep]
      for "End rep", connection changes, settings changes).

    It then acts on the returned {!event}s: append [Sample]s and grip marks
    to a {!Force_buffer}, play {!Sound.tones}, save sets, open the review,
    show the battery. Everything else it shows (phase, timer, deviation,
    live mean/SD, measured load) it reads from the state.

    Times: [now] is [performance.now()] in ms; sample and grip times are page
    seconds ([now / 1000] and the {!Clock} mapping); rep durations are device
    seconds ({!Clock.time}, fix 3). Timers the MVP ran with
    [setInterval] / [setTimeout] (the off-target alert every 500 ms, the
    second set-complete tone 180 ms later) become state checked on each
    input ([alert_due]) or delays in {!Sound.tones}.

    {2 Weighing}

    Outside a set, the Progressor doubles as a scale, so a load can be
    weighed and offered as the target ("Use as target", enabled once the
    load is off):

    - Idle and not in a set, a sample of {!weigh_threshold} (3 kg) or more
      starts a hold: [measured] is that sample at first.
    - While samples stay at 3 kg or more, the last {!weigh_window} (2 s) are
      kept, and on every 8th sample of the hold (every 0.1 s) [measured]
      becomes their median.
    - The first sample under 3 kg ends the hold: [measured] becomes the
      median of the middle 40 % (by time) of that 2 s window, so neither the
      lift-off nor (in a hold under 2 s) the hang-on is in it (fewer than 5
      samples: their median). It stays until the next hold or a set starts.
    - A sample of 3 kg or more right after one under starts a new hold;
      another sample under 3 kg returns to idle. Starting a set clears
      [measured]; inside a set nothing is weighed.

    {2 Behaviour kept from the MVP on purpose}

    - A rep starts at GO, whether or not the user is gripping, and ends the
      first time force drops below the disengage threshold {e after} having
      reached the engage threshold. A rep that never reaches the engage
      threshold never ends on its own: [End_rep] ends it and keeps it (see
      "End rep" below); [End_set] ends it and drops it.
    - Thresholds and the tolerance band are read from the {e current}
      settings on every sample, so editing them mid-set would take effect at
      once. The owner's decision (2) is to keep this and lock those settings
      in the UI instead: the Timer disables them during a set and holds them
      with {!Settings.keep_set_fields}.
    - Outside a set, any sample of 3 kg or more starts weighing, including
      samples right after a set ends while the hand is still on the gripper.
    - The off-target alert's 500 ms beat restarts each time force leaves the
      tolerance band again, instead of running on a fixed grid: leaving the
      band beeps at once if the last alert was 500 ms or more ago, then every
      500 ms from that moment. Beeps are never under 500 ms apart, but force
      that wavers across the band edge gets them as often as that, at an
      uneven rhythm. The owner prefers this to a fixed beat.

    {2 End rep (new; the MVP has no such control)}

    [End_rep], valid only in the [Rep] phase (ignored in the countdown, the
    rest and outside a set), ends the rep at that moment exactly as if force
    had just dropped below the disengage threshold, whether or not it ever
    reached the engage threshold:

    - the rep is recorded: its duration runs from GO to the press, both
      placed on the device clock the same way ({!Clock.device_at}, fix 3);
      when either can't be placed (no notification yet on the current
      anchor, e.g. right after a reconnection) both are taken on the page
      clock instead;
    - its stats ({!Rep.make}) come from the samples received since GO, all
      of them, as for a rep that ends on its own (which also keeps the
      samples from before it engaged). A rep that never engaged is kept
      with whatever force it had (a light hold's median, mean and SD); one
      with no samples at all (the Progressor sent nothing) gets [0] for
      every force statistic and an empty graph. Samples taken before the
      press that only arrive after it (notifications come every ~200 ms)
      are not in it;
    - then the same as a natural rep end: [Grip_ended] (at the press),
      [Rep_finished], [Save_set], and [Rest_started], or [Set_complete] and
      [Set_ended] if it was the last planned rep, with the same tones.

    {2 Deliberate differences from the MVP}

    Fixes the owner decided on after the port, numbered as in that review;
    everything else is as the MVP does it (the port was checked bit for bit
    against the MVP's JavaScript with exactly these fixes applied).

    - (3) Rep durations are measured on one clock, the Progressor's: from
      GO, placed on the device clock with the smallest recent offset between
      the two clocks ({!Clock.device_at}), to the ending sample's device time.
      The same GO decides which late-arriving samples were taken before it.
      The MVP subtracted GO's page time from the ending sample's {e mapped}
      page time, so whatever the mapping was off by at that moment went into
      the rep: its average radio delay always (~15 ms more than the fastest
      notification's in the test traces), and after a radio stall,
      whose recovery pushes the mapping ahead by up to the stall's length,
      up to that much (0.4 s and 1.4 s in two traces), easing off over
      ~10 s.
    - (4) Device counter restarts ({!Clock}): a restart (tare, reconnect)
      re-anchors so that the notification's newest sample lands at now, and
      a uint32 wrap is not treated as a restart. The MVP pinned the first
      sample after a restart (or wrap) at now, so the rest of that
      notification landed up to 187.5 ms in the future, and the drift easing
      took ~10 s to absorb it; rep times and the graph were off by what was
      left of it.
    - (6) A finished set is saved once per rep and not again when the set
      ends: the MVP's [endSet] saved the set a second time although every
      rep had already saved it as it finished.
    - (8) Weighing (above) keeps a sliding window of the hold's last 2 s.
      The MVP took the median of every sample since the load went on,
      re-sorting all of them every 8th sample, and at release the trimmed
      median of the whole hold; so a change of load mid-hold (adding a
      plate) showed only once the new load made up half the hold, and the
      value kept at release came from the middle 40 % of the whole hold,
      which in a long hold can be well before its end. Holds of 2 s or less
      give the same values as before.

    Elsewhere: (7) {!History.pc_shown} (an interrupted PowerCurve upload is
    never shown as uploading); (2) {!Settings.keep_set_fields} (see above). *)

type phase = Off | Countdown | Rep | Rest
(** [SET.phase]. *)

type handler = Idle | Repping | Weighing
(** [P.state]. *)

type t = private {
  settings : Settings.t;
  clock : Clock.t;
  awaiting_battery : bool;
  (* Set flow (SET) *)
  phase : phase;
  phase_start : float;  (** ms. *)
  phase_end : float;  (** ms: [phase_start + seconds · 1000]. *)
  phase_total : float;  (** Seconds the countdown/rest lasts ([0.] otherwise). *)
  last_beep : int option;  (** Countdown second last beeped. *)
  current : History.set option;  (** The set in progress, with its finished reps. *)
  (* Force handler (P) *)
  handler : handler;
  go : float;  (** Page seconds of the current rep's GO. *)
  go_device : float option;
      (** GO on the device clock ({!Clock.device_at}; if no notification had
          arrived on the current anchor, placed by the first sample after GO
          instead); the rep's duration is measured from it (fix 3). *)
  armed : bool;  (** The rep has reached the engage threshold. *)
  holding : bool;  (** Weighing: a load is on. *)
  samples : float list;
      (** Rep samples, or the weighing window (at most {!weigh_window}), newest
          first. *)
  sample_count : int;  (** Samples taken in this rep or this hold. *)
  running : Stats.Running.t;
  force : float;  (** Latest raw sample, kg ([P.current]). *)
  measured : float option;
      (** Weighed load, kg, offered as a target (see "Weighing" above). *)
  off_target : bool;
  off_dir : float option;  (** Latest force − target while off target. *)
  last_alert : float;  (** ms of the last off-target alert ([neg_infinity] at first). *)
  alert_due : float option;  (** ms the repeating off-target alert fires next. *)
  live_mean : float option;
  live_sd : float option;
}

val init : Settings.t -> t

type input =
  | Packet of { now : float; view : Protocol.view }
  | Samples of { now : float; samples : Protocol.sample array }
      (** Already decoded weight samples (bypasses {!Protocol.parse}). *)
  | Tick of { now : float }
  | Start_set of { now : float; wall_ms : float;  (** [Date.now()] *) session_uuid : string }
  | End_set of { now : float }  (** "End set early". *)
  | End_rep of { now : float }
      (** "End rep": end the current rep now (see "End rep" above); ignored
          outside the [Rep] phase. *)
  | Connected of { now : float; fresh : bool }
      (** Notifications are on. [fresh]: a new pairing (clears the graph);
          [false]: an automatic reconnect. Then send
          {!Protocol.connect_commands}. *)
  | Disconnected of { now : float }  (** The user disconnected (ends a set in progress). *)
  | Settings_changed of Settings.t

type event =
  | Battery of int  (** Percent. *)
  | Sample of { t : float; force : float }  (** Append to the graph buffer. *)
  | Buffer_cleared
  | Grip_started of float  (** Shade from this page time (GO). *)
  | Grip_ended of float  (** Close the open shading here. *)
  | Grip_discarded  (** Remove the open shading (rep ended without engaging). *)
  | Countdown_beep of int  (** 5 … 1 seconds left. *)
  | Rep_started  (** GO. *)
  | Rep_finished of Rep.t
  | Rest_started
  | Set_complete  (** The planned reps are done (the set ends right after). *)
  | Off_target_alert of { heavy : bool; at : float  (** ms *) }
  | Save_set of History.set
      (** Upsert into history and persist: once per finished rep, with every
          rep so far (fix 6: not again when the set ends). *)
  | Set_ended of History.set option
      (** The set that just ended; open the review when it has reps. *)

val step : t -> input -> t * event list

(** {2 Derived values for the readout} *)

val in_set : t -> bool
val seconds_left : t -> now:float -> float
(** Countdown/rest seconds left, [≥ 0]. *)

val progress : t -> now:float -> float
(** Fraction of the countdown/rest left (the readout's bar); [0.] otherwise. *)

val rep_elapsed : t -> now:float -> float
(** Seconds since GO, [≥ 0]. *)

val reps_done : t -> int
val planned_reps : t -> int
(** Of the set in progress, else the settings'. *)

val alert_interval : float
(** 500 ms. *)

val weigh_threshold : float
(** 3 kg. *)

val weigh_window : int
(** 160 samples: 2 s at 80 Hz. *)
