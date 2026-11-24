(** IndexedDB as promise-returning key-value stores ({!Timer_db} keeps the
    recorded sets in one).

    Nothing here rejects or raises. Where IndexedDB is missing or refuses
    (private modes, full storage, embedded views), opening answers [None], reads
    answer nothing and writes answer [false], so callers degrade to "nothing is
    kept on this device". *)

type t

val open_ : name:string -> stores:string list -> t option Js.Promise.t
(** Opens (creating it, with these object stores, on first use) the database
    [name]. *)

val close : t -> unit

val delete_database : string -> unit Js.Promise.t
(** Resolves once the database is gone, can't be deleted, or is still open
    elsewhere (it is then deleted when that connection closes). *)

val get : t -> store:string -> string -> Js.Json.t option Js.Promise.t
val get_all : t -> store:string -> Js.Json.t array Js.Promise.t

type op =
  | Put of { store : string; key : string; value : Js.Json.t }
  | Delete of { store : string; key : string }

val write : t -> op list -> bool Js.Promise.t
(** All of [ops] in one transaction, resolved when it has committed. [false]:
    nothing was written. *)
