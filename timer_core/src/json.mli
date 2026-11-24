(** A dependency-free JSON value, parser and printer that match
    [JSON.parse] / [JSON.stringify] closely enough to read and rewrite the
    MVP's localStorage byte for byte.

    {2 Why not bind [JSON.parse] under Melange?}

    The library has to build natively too (its tests), so a
    binding would need a second, native implementation behind a virtual
    module, and the two could drift. The data is small (a settings object and
    a history of a few hundred sets), so a hand-written parser costs nothing
    noticeable, and one implementation means native and Melange results are
    identical by construction. The UI may still use [JSON.parse] itself and
    convert, but has no need to.

    {2 Strings}

    Strings are passed through without decoding: natively they are UTF-8
    bytes, under Melange they are JS (UTF-16) strings, and unescaped
    characters are copied verbatim either way. [\uXXXX] escapes are decoded to
    UTF-8 natively and to the UTF-16 code unit under Melange (chosen with
    [Sys.backend_type]). The printer escapes exactly what [JSON.stringify]
    escapes: the double quote, the backslash, control characters below U+0020 ([\b \f \n \r \t] by
    name, the rest as [\u00XX]) and, under Melange, lone surrogates.

    {2 Numbers}

    Numbers are floats; they print as {!Jsnum.to_string} does, and non-finite
    numbers print as [null], as in JS. *)

type t =
  | Null
  | Bool of bool
  | Number of float
  | String of string
  | Array of t list
  | Object of (string * t) list  (** In source order; duplicates kept. *)

val parse : string -> (t, string) result
(** Strict RFC 8259 parsing, as [JSON.parse] (surrounding whitespace
    allowed). *)

val to_string : ?indent:int -> t -> string
(** [JSON.stringify(v)], or [JSON.stringify(v, null, indent)] with
    [indent > 0] (empty arrays and objects stay ["[]"] / ["{}"]). *)

(** {2 Access helpers} *)

val member : string -> t -> t option
(** The value of a key in an object (the last one if repeated, as
    [JSON.parse] keeps); [None] for a missing key or a non-object. *)

val to_number : t -> float option
val to_bool : t -> bool option
val to_string_opt : t -> string option
val to_list : t -> t list option
val equal : t -> t -> bool
(** Structural equality; numbers compare with [=] (so [nan <> nan]),
    object keys in order. *)
