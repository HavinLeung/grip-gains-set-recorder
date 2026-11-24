(** JavaScript number semantics the MVP relies on, reproduced bit for bit so
    native and Melange builds agree with the original page.

    Every float operation in this library mirrors the order of operations in
    [public/index.html]; these helpers cover the cases where OCaml's stdlib
    differs from JS: rounding ties, unsigned 32-bit wrap-around, and the
    textual forms of numbers ([Number.prototype.toString], [toFixed]). *)

val round : float -> float
(** [Math.round]: nearest integer, ties toward +∞ (so [round (-2.5) = -2.]),
    unlike [Float.round] which rounds ties away from zero. *)

val r2 : float -> float
(** [Math.round(x * 100) / 100], the MVP's two-decimal rounding of stored
    statistics. *)

val to_uint32 : float -> float
(** [x >>> 0]: ToUint32, returned as a float so it is exact under Melange
    (whose [int] is 32-bit signed). Non-finite inputs give [0.]. *)

val to_string : float -> string
(** [String(x)] / [JSON.stringify(x)] for finite [x]: the shortest decimal
    that round-trips, in fixed notation for [1e-7 ≤ |x| < 1e21] and
    exponent notation ([1.5e-7], [1e+21]) otherwise. [-0.] prints as ["0"],
    NaN as ["NaN"], infinities as ["Infinity"] / ["-Infinity"]. *)

val to_fixed : float -> int -> string
(** [x.toFixed(digits)] for [0 ≤ digits ≤ 20]: rounds the exact binary value
    half-up (so [to_fixed 0.25 1 = "0.3"], where [Printf "%.1f"] gives
    ["0.2"]), keeps a ["-"] on negative values that round to zero
    ([to_fixed (-0.04) 1 = "-0.0"]), and falls back to {!to_string} for
    [|x| ≥ 1e21]. *)

val to_base36 : float -> string
(** [n.toString(36)] for a non-negative integer-valued [n < 2^53]; used for
    set ids ([Date.now().toString(36)]). *)

val iso_utc : float -> string
(** The MVP's [isoUtc]: [new Date(ms).toISOString()] with the [.sssZ]
    suffix replaced by [+00:00], e.g. ["2026-11-23T08:47:12+00:00"]. [ms] is
    milliseconds since the Unix epoch (years 0–9999). *)

val signed : float -> int -> string
(** The MVP's [signed(v, d)]: ["+"] for [v ≥ 0] (including [-0.]), the minus
    sign U+2212 ["−"] otherwise, then [|v|.toFixed(d)]. *)

val capitalize : string -> string
(** [cap(s)]: first character upper-cased (ASCII). *)
