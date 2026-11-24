(** Mapping the Progressor's microsecond counter onto the page clock
    (seconds, [performance.now() / 1000]), as the MVP's [onSamples] /
    [processSample] do, with fix 4 (counter restarts) applied.

    The device sends ~16 samples every 200 ms. Each sample's page time is
    [anchor.page + ((device_us − anchor.device) >>> 0) / 1e6]. A new anchor
    is placed so that the notification's newest sample lands at "now" (never
    less than 12.5 ms after the previous sample), and after every
    notification it is nudged so the newest sample keeps tracking "now":

    - lead = last sample's page time − now;
    - if lead < −0.6 s (a radio stall: the samples are old), the anchor jumps
      forward by −lead;
    - otherwise it eases by [lead · 0.05] (absorbs crystal drift).

    {2 Counter restarts (fix 4)}

    A sample continues the current anchor when the counter moved forward
    (modulo 2{^32}: a wrap, every 71.6 minutes, is just more time) and the
    mapping does not put it more than {!max_lead} ahead of now. Otherwise the
    counter restarted (a tare or a reconnect restarts it, or the device was
    switched off and on) and that sample gets a new anchor, placed like a
    notification's first anchor: so that the notification's newest sample
    lands at now, counting device time within each run of consecutive samples
    and 12.5 ms across a break. An anchor is only made when needed: after
    {!reset_anchor} the first notification is anchored once and kept.

    The MVP instead dropped the anchor whenever the counter was lower than
    the previous sample's (also at a wrap, and also right after a
    reconnection, discarding the anchor it had just placed), and re-anchored
    that sample at "now" itself. The samples after it in the same
    notification then landed up to 187.5 ms in the future, and the 5 %
    easing took ~10 s to work that off.

    {2 Device time (fix 3)}

    Besides the page time, every sample gets a {e device time}: seconds on
    the Progressor's own clock, continuous across counter wraps, restarts and
    reconnections. Between samples on one anchor it advances by exactly the
    counter's difference; across a new anchor (a restart or reconnection,
    where the counter says nothing) by the difference of the two samples'
    page times. Unlike page times it is untouched by the drift easing and the
    stall jump, so differences of device times are the device's own
    measurement of elapsed time (rep durations use them).

    To place a page-clock instant (GO) on the device clock, {!device_at} uses
    the offset between the clocks seen by the earliest-arriving of the last
    25 notifications (~5 s): [now − device time of its newest sample].
    Bluetooth delays only ever make a notification late, so the smallest
    offset is the closest to the truth: it is off by that notification's
    transmission delay only, a few ms, whatever stalls or drift easing did
    to the page mapping. *)

type anchor = { device : float;  (** µs *) page : float  (** s *) }

type t = private {
  anchor : anchor option;  (** [P.firstDev] / [P.firstDisp]. *)
  last_page : float option;  (** [P.lastDisp]: page time of the last sample. *)
  last_device : float;  (** [P.lastTs]: never reset, initially [0.]. *)
  device_s : float;  (** Device time of the last sample, s ([0.] before any). *)
  offsets : float list;
      (** [now − device time] of each recent notification's newest sample,
          newest first, at most 25; emptied by a new anchor. *)
}

type time = { page_s : float;  (** Page time, s. *) device_s : float  (** Device time, s. *) }

val empty : t

val reset_anchor : t -> t
(** [resetTiming()]: on every (re)connection (also forgets the offsets). *)

val forget_last : t -> t
(** [P.lastDisp = null]: on a fresh connection (with {!reset_anchor}). *)

val max_lead : float
(** 10 s: a sample the current anchor would put further ahead of now is taken
    as a counter restart. Far beyond any lead a stall leaves behind (a stall
    longer than the Bluetooth supervision timeout, at most 6 s on iOS, drops
    the connection), and it catches a restart that reads as a jump forward
    modulo 2{^32} (the old counter was near the top of its range). *)

val device_at : t -> page_s:float -> float option
(** A page time on the device clock, from the smallest recent offset; [None]
    before the first notification on the current anchor. *)

val map_batch : t -> now_s:float -> Protocol.sample array -> t * time array
(** Page and device times for one notification's samples, in order, and the
    clock after the end-of-notification easing. *)
