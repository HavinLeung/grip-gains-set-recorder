type factory
type t
type request
type transaction
type object_store

(* [globalThis.indexedDB]: undefined in some embedded or locked-down browsers. *)
external factory : factory Js.undefined = "indexedDB" [@@mel.scope "globalThis"]
external open_db : factory -> string -> int -> request = "open" [@@mel.send]

external delete_db : factory -> string -> request = "deleteDatabase"
[@@mel.send]

external database : request -> t = "result" [@@mel.get]
external result : request -> Js.Json.t Js.undefined = "result" [@@mel.get]

external results : request -> Js.Json.t array Js.undefined = "result"
[@@mel.get]

external on_success : request -> (unit -> unit) -> unit = "onsuccess"
[@@mel.set]

external on_error : request -> (unit -> unit) -> unit = "onerror" [@@mel.set]

external on_blocked : request -> (unit -> unit) -> unit = "onblocked"
[@@mel.set]

external on_upgrade_needed : request -> (unit -> unit) -> unit
  = "onupgradeneeded"
[@@mel.set]

external has_store : t -> string -> bool = "contains"
[@@mel.send] [@@mel.scope "objectStoreNames"]

external create_store : t -> string -> object_store = "createObjectStore"
[@@mel.send]

external close_db : t -> unit = "close" [@@mel.send]

external transaction : t -> string array -> string -> transaction
  = "transaction"
[@@mel.send]

external on_complete : transaction -> (unit -> unit) -> unit = "oncomplete"
[@@mel.set]

external on_tx_error : transaction -> (unit -> unit) -> unit = "onerror"
[@@mel.set]

external on_abort : transaction -> (unit -> unit) -> unit = "onabort"
[@@mel.set]

external object_store : transaction -> string -> object_store = "objectStore"
[@@mel.send]

external store_get : object_store -> string -> request = "get" [@@mel.send]
external store_get_all : object_store -> request = "getAll" [@@mel.send]

external store_put : object_store -> Js.Json.t -> string -> request = "put"
[@@mel.send]

external store_delete : object_store -> string -> request = "delete"
[@@mel.send]

(* A promise settled by whichever callback fires first; later calls do nothing. *)
let settle (start : ('a -> unit) -> unit) : 'a Js.Promise.t =
  Js.Promise.make (fun ~resolve ~reject:_ -> start (fun v -> (resolve v [@u])))

let open_ ~name ~stores =
  match Js.Undefined.toOption factory with
  | None -> Js.Promise.resolve None
  | Some f ->
      settle (fun finish ->
          match open_db f name 1 with
          | exception _ -> finish None
          | req ->
              on_upgrade_needed req (fun () ->
                  let db = database req in
                  List.iter
                    (fun s ->
                      if not (has_store db s) then ignore (create_store db s))
                    stores);
              on_success req (fun () -> finish (Some (database req)));
              on_error req (fun () -> finish None);
              on_blocked req (fun () -> finish None))

let close db = try close_db db with _ -> ()

let delete_database name =
  match Js.Undefined.toOption factory with
  | None -> Js.Promise.resolve ()
  | Some f ->
      settle (fun finish ->
          match delete_db f name with
          | exception _ -> finish ()
          | req ->
              on_success req finish;
              on_error req finish;
              (* Another tab still has it open: it is deleted once that tab lets go. *)
              on_blocked req finish)

let read db ~store ~none (ask : object_store -> request) get =
  settle (fun finish ->
      match
        ask (object_store (transaction db [| store |] "readonly") store)
      with
      | exception _ -> finish none
      | req ->
          on_success req (fun () -> finish (get req));
          on_error req (fun () -> finish none))

let get db ~store key =
  read db ~store ~none:None
    (fun s -> store_get s key)
    (fun req -> Js.Undefined.toOption (result req))

let get_all db ~store =
  read db ~store ~none:[||] store_get_all (fun req ->
      Option.value ~default:[||] (Js.Undefined.toOption (results req)))

type op =
  | Put of { store : string; key : string; value : Js.Json.t }
  | Delete of { store : string; key : string }

let store_of = function Put { store; _ } | Delete { store; _ } -> store

let write db ops =
  let stores = List.sort_uniq String.compare (List.map store_of ops) in
  if stores = [] then Js.Promise.resolve true
  else
    settle (fun finish ->
        match transaction db (Array.of_list stores) "readwrite" with
        | exception _ -> finish false
        | tx -> (
            on_complete tx (fun () -> finish true);
            on_tx_error tx (fun () -> finish false);
            on_abort tx (fun () -> finish false);
            try
              List.iter
                (function
                  | Put { store; key; value } ->
                      ignore (store_put (object_store tx store) value key)
                  | Delete { store; key } ->
                      ignore (store_delete (object_store tx store) key))
                ops
            with _ -> finish false))
