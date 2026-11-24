(* Where sets are kept: one IndexedDB database in this browser.

   - "sets": one small record per set: its id, its summary (Timer_summary: weight, hold times,
     per-rep statistics) and whether it is still being recorded.
   - "traces": the whole set (Timer_core.History JSON: per-rep stats, percentiles and the 20 Hz
     force trace of every rep). A set recorded in manual mode has no trace (there is no force);
     its record is all there is.

   Nothing here leaves the browser. *)

open Timer_core

type status =
  | Recording
      (** saved after each rep; the end-of-set review hasn't been answered yet
      *)
  | Saved  (** confirmed in the review (or left unreviewed by a closed page) *)

type record = {
  id : string;
  status : status;
  summary : Js.Json.t;  (** {!Timer_summary.of_set} *)
  recorded_at : float;  (** ms; when the set started *)
  updated_at : float;
  pc : string option;
      (** PowerCurve upload: "uploaded" | "failed" | "uploading", for the
          history list *)
  pc_state : string option;
      (** a manual set's PowerCurve upload state (History's JSON, as text): a
          manual set has no trace, where the state otherwise lives *)
}

type t = Idb.t

let database_name = "grip-gains-set-recorder-v1"
let sets_store = "sets"
let traces_store = "traces"
let ( let* ) p f = Js.Promise.then_ f p
let return = Js.Promise.resolve

(* --- record <-> JSON --- *)

let status_id = function Recording -> "recording" | Saved -> "saved"

let record_to_json r =
  let opt = function Some p -> Js.Json.string p | None -> Js.Json.null in
  Js.Json.object_
    (Js.Dict.fromList
       ([
          ("id", Js.Json.string r.id);
          ("status", Js.Json.string (status_id r.status));
          ("summary", r.summary);
          ("recorded_at", Js.Json.number r.recorded_at);
          ("updated_at", Js.Json.number r.updated_at);
          ("pc", opt r.pc);
        ]
       @
       match r.pc_state with
       | Some p -> [ ("pc_state", Js.Json.string p) ]
       | None -> []))

let record_of_json json =
  let ( let* ) = Option.bind in
  let* id = Decode.string_field "id" json in
  let* s = Decode.string_field "status" json in
  let* summary = Decode.field "summary" Js.Json.decodeObject json in
  let* recorded_at = Decode.number_field "recorded_at" json in
  let updated_at =
    Option.value ~default:recorded_at (Decode.number_field "updated_at" json)
  in
  let* status =
    match s with
    | "recording" -> Some Recording
    | "saved" -> Some Saved
    | _ -> None
  in
  Some
    {
      id;
      status;
      summary = Js.Json.object_ summary;
      recorded_at;
      updated_at;
      pc = Decode.string_field "pc" json;
      pc_state = Decode.string_field "pc_state" json;
    }

(* --- opening --- *)

let open_ () : t option Js.Promise.t =
  Idb.open_ ~name:database_name ~stores:[ sets_store; traces_store ]

(* --- reads --- *)

let records db : record list Js.Promise.t =
  let* all = Idb.get_all db ~store:sets_store in
  return (Array.to_list all |> List.filter_map record_of_json)

(* The set as recorded (with its rep traces), if it is here. *)
let trace db id : History.set option Js.Promise.t =
  let* stored = Idb.get db ~store:traces_store id in
  return
    (match Option.bind stored Js.Json.decodeString with
    | Some text -> (
        match Json.parse text with
        | Ok j -> Result.to_option (History.set_of_json j)
        | Error _ -> None)
    | None -> None)

(* --- writes: one transaction, resolved when committed; [false] if it failed --- *)

type write =
  | Put_record of record
  | Put_trace of string * History.set
  | Delete of string

let write (db : t) (ops : write list) : bool Js.Promise.t =
  Idb.write db
    (List.concat_map
       (function
         | Put_record r ->
             [
               Idb.Put
                 { store = sets_store; key = r.id; value = record_to_json r };
             ]
         | Put_trace (id, set) ->
             [
               Idb.Put
                 {
                   store = traces_store;
                   key = id;
                   value =
                     Js.Json.string (History.set_to_json set |> Json.to_string);
                 };
             ]
         | Delete id ->
             [
               Idb.Delete { store = sets_store; key = id };
               Idb.Delete { store = traces_store; key = id };
             ])
       ops)
