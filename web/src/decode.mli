(** Reading fields out of JSON objects (stored records). Everything answers
    [None] instead of raising when the JSON isn't what was expected. *)

val field : string -> (Js.Json.t -> 'a option) -> Js.Json.t -> 'a option
(** [field name decode json]: [json]'s member [name], decoded. *)

val string_field : string -> Js.Json.t -> string option
val number_field : string -> Js.Json.t -> float option
val bool_field : string -> Js.Json.t -> bool option

val int_field : string -> Js.Json.t -> int option
(** A number that is a whole number. *)

val nullable :
  string -> (Js.Json.t -> 'a option) -> Js.Json.t -> 'a option option
(** A member that may be [null]: [Some None] for null, [None] if it is missing
    or of the wrong type. *)

val array : (Js.Json.t -> 'a option) -> Js.Json.t -> 'a array option
(** Every element must decode. *)
