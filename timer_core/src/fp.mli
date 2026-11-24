(** Float multiplication that is never fused into a multiply-add.

    OCaml's arm64 native backend compiles [a *. b +. c] (and [-.]) to a
    single [fmadd]/[fmsub] instruction, rounding once where JS rounds the
    product and the sum separately; the last bit then differs from the MVP.
    Every module that does float arithmetic opens this, which shadows
    [( *. )] with a version whose result passes through
    [Sys.opaque_identity], so the product is always rounded on its own.
    (Bytecode and Melange never fuse; there it is plain multiplication.) *)

val ( *. ) : float -> float -> float
