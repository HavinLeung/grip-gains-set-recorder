(** The pure core of the Grip Gains set timer: everything the MVP page (the
    app's first version, a single HTML page with inline JavaScript) computes, without DOM, Bluetooth, audio, storage or
    clocks. Times and inputs are passed in; results are values and events.

    Pure OCaml stdlib (no C stubs, no Unix); builds natively and with Melange,
    with identical results.

    - {!Protocol}: Progressor command bytes, notification parsing (weight
      samples, battery millivolts) and the battery curve.
    - {!Clock}: device µs timestamps → page seconds, with drift easing and
      stall recovery, and device seconds for timing reps.
    - {!Session}: the force handler and set flow as one state with a pure
      [step : t -> input -> t * event list].
    - {!Rep}, {!Stats}, {!Summary}: rep analysis (stable region, median,
      mean, SD, percentiles, deviation), Welford live stats, set summary.
    - {!Force_buffer}, {!Graph}: the live graph's history buffer (the one
      mutable structure) and its draw-ready geometry; saved rep graphs.
    - {!Settings}, {!History}, {!Json}: typed settings and saved sets, with
      JSON compatible with the MVP's localStorage ([grip-gains-force.v2],
      [grip-gains-force.history.v1]).
    - {!Sound}: the tones each event plays.
    - {!Jsnum}: JS number semantics ([Math.round], [>>> 0], [toFixed],
      [String(x)]) the above rely on.

    {2 Design choices}

    - Every computation follows the JS's order of float operations, so the
      results are bit-identical to the MVP with the owner's decided fixes
      (listed in {!Session}, "Deliberate differences from the MVP").
    - Bytes are read through {!Protocol.view} (length + byte getter), built
      by the UI straight from a [DataView]; multi-byte fields are decoded
      arithmetically and uint32 values are floats, since Melange's [int] is
      32-bit.
    - JSON is a small hand-written codec ({!Json}), the same code on both
      targets; see its docs for the reasoning.
    - Timers become state: the off-target alert's [setInterval] is
      [Session.t.alert_due], checked on every input (the UI sends [Tick]
      every 100 ms); the set-complete tone's [setTimeout] is a delay in
      {!Sound.tones}.
    - [Start_set] while a set runs and [End_set] outside one are ignored (the
      MVP's single button toggles between them).
    - [End_rep] (new, no MVP equivalent) is ignored outside a rep.
    - Native arm64 code would fuse [a *. b +. c] into one rounding; {!Fp}
      prevents it so native results match JS bit for bit.

    {2 Tests} ([dune test])

    - [test/test_timer_core.ml]: XCTest cases ported from the Swift app
      (statistics, RepResult, SetStatistics, battery, thresholds, off-target,
      weighing, timestamp restarts, countdown beeps), adapted where the MVP
      deliberately differs, plus hand-written cases. *)

module Fp = Fp
module Jsnum = Jsnum
module Json = Json
module Stats = Stats
module Rep = Rep
module Protocol = Protocol
module Clock = Clock
module Settings = Settings
module History = History
module Summary = Summary
module Session = Session
module Force_buffer = Force_buffer
module Graph = Graph
module Sound = Sound
