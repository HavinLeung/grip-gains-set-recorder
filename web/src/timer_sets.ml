(* The sets recorded in this browser. Every set is written to the device (Timer_db) as it is
   recorded and stays there until it is deleted; nothing is sent anywhere.

   Life of a set:
   - after each rep the core emits [Save_set]: the set (and its traces) is written with status
     [Recording], so closing the page mid-set loses nothing;
   - the end-of-set review answers it: Save marks it [Saved] (with the corrected weight),
     Discard deletes it;
   - a page that closed before the review was answered leaves [Recording] sets behind: on the
     next start they are kept as recorded.

   All writes run one after another on a single queue, so a delete can't overtake the write of
   the same set.

   Without device storage (IndexedDB missing or refused) everything still works for as long as
   the page stays open: sets are kept in memory. *)

open Timer_core

type callbacks = {
  on_change : unit -> unit;
  on_store_error : unit -> unit;
  log : string -> unit;
}

type t = {
  callbacks : callbacks;
  mutable db : Timer_db.t option;
  mutable records : Timer_db.record list;  (** in memory, oldest first *)
  by_id : Timer_db.record Js.Dict.t;  (** the same records, by id *)
  mutable chain : unit Js.Promise.t;  (** the serial queue *)
}

let ( let* ) p f = Js.Promise.then_ f p
let return = Js.Promise.resolve
let now = Browser.date_now

(* Runs [f] after everything queued before it. *)
let enqueue t (f : unit -> 'a Js.Promise.t) : 'a Js.Promise.t =
  let result = Js.Promise.then_ (fun () -> f ()) t.chain in
  t.chain <-
    Js.Promise.catch
      (fun _ -> return ())
      (Js.Promise.then_ (fun _ -> return ()) result);
  result

let sort_records l =
  List.sort
    (fun (a : Timer_db.record) b -> Float.compare a.recorded_at b.recorded_at)
    l

let set_record t (r : Timer_db.record) =
  Js.Dict.set t.by_id r.id r;
  t.records <-
    sort_records
      (r :: List.filter (fun (x : Timer_db.record) -> x.id <> r.id) t.records)

let drop_record t id =
  ignore (Browser.dict_delete t.by_id id);
  t.records <- List.filter (fun (x : Timer_db.record) -> x.id <> id) t.records

let find t id = Js.Dict.get t.by_id id
let records t = t.records

let write t ops =
  match t.db with Some db -> Timer_db.write db ops | None -> return false

(* --- the set life cycle --- *)

let record_of ~status ~manual (set : History.set) =
  {
    Timer_db.id = Timer_summary.id_of_set set;
    status;
    summary = Timer_summary.of_set ~manual set;
    recorded_at = Timer_summary.performed_at set;
    updated_at = now ();
    pc = None;
    pc_state = None;
  }

(* A manual set has no trace to keep (no force was measured): its record is all there is. *)
let with_trace ~manual (set : History.set) (r : Timer_db.record) =
  if manual then [ Timer_db.Put_record r ]
  else [ Timer_db.Put_record r; Put_trace (r.id, set) ]

(* After each rep: keep what has been recorded so far. *)
let save_recording t ~manual (set : History.set) =
  let r = record_of ~status:Recording ~manual set in
  set_record t r;
  enqueue t (fun () ->
      Js.Promise.then_
        (fun ok ->
          if not ok then t.callbacks.on_store_error ();
          return ok)
        (write t (with_trace ~manual set r)))

(* The review's Save: the set as corrected. *)
let confirm t ~manual (set : History.set) =
  let r = record_of ~status:Saved ~manual set in
  set_record t r;
  t.callbacks.on_change ();
  let* ok = enqueue t (fun () -> write t (with_trace ~manual set r)) in
  if not ok then begin
    t.callbacks.log
      "Couldn't save the set on this device (storage refused); it is kept \
       until the page closes";
    t.callbacks.on_store_error ()
  end;
  return ok

(* The review's Discard. *)
let discard t (set : History.set) =
  let id = Timer_summary.id_of_set set in
  drop_record t id;
  t.callbacks.on_change ();
  enqueue t (fun () -> write t [ Delete id ])

(* PowerCurve state lives in the trace. *)
let pc_label (set : History.set) =
  match set.pc with
  | Some History.Uploading -> Some "uploading"
  | Some (History.Uploaded _) -> Some "uploaded"
  | Some (History.Failed _) -> Some "failed"
  | _ -> None

let summary (r : Timer_db.record) = Timer_summary.of_json r.summary

(* A manual set's record: it has no trace. *)
let is_manual (r : Timer_db.record) =
  match summary r with Some sm -> sm.manual | None -> false

let update_trace t id (set : History.set) =
  let ops =
    match find t id with
    | Some r when is_manual r ->
        (* no trace: the state goes in the record *)
        let r' =
          {
            r with
            pc = pc_label set;
            pc_state = Option.map Timer_summary.pc_to_string set.pc;
          }
        in
        set_record t r';
        [ Timer_db.Put_record r' ]
    | Some r ->
        let r' = { r with pc = pc_label set } in
        set_record t r';
        [ Timer_db.Put_record r'; Put_trace (id, set) ]
    | None -> [ Timer_db.Put_trace (id, set) ]
  in
  t.callbacks.on_change ();
  enqueue t (fun () -> write t ops)

let trace t id : History.set option Js.Promise.t =
  let manual =
    Option.bind (find t id) (fun (r : Timer_db.record) ->
        Option.bind (summary r) (fun sm ->
            if sm.manual then
              Some
                (Timer_summary.manual_set sm
                   ~pc:(Option.bind r.pc_state Timer_summary.pc_of_string))
            else None))
  in
  match (manual, t.db) with
  | Some set, _ -> return (Some set)
  | None, Some db -> Timer_db.trace db id
  | None, None -> return None

let delete t id : unit Js.Promise.t =
  drop_record t id;
  t.callbacks.on_change ();
  let* _ = enqueue t (fun () -> write t [ Delete id ]) in
  return ()

let create callbacks =
  {
    callbacks;
    db = None;
    records = [];
    by_id = Js.Dict.empty ();
    chain = return ();
  }

(* Opens the device storage and reads what it holds. *)
let start t : unit Js.Promise.t =
  let* db = Timer_db.open_ () in
  t.db <- db;
  if db = None then
    t.callbacks.log
      "This browser won't let the page keep sets on the device (IndexedDB \
       unavailable)";
  let* records =
    match db with Some d -> Timer_db.records d | None -> return []
  in
  let leftover, rest =
    List.partition (fun (r : Timer_db.record) -> r.status = Recording) records
  in
  let promoted =
    List.map
      (fun (r : Timer_db.record) ->
        { r with status = Saved; updated_at = now () })
      leftover
  in
  (* Merge with anything recorded in the meantime (in memory wins). *)
  let fresh =
    List.filter
      (fun (r : Timer_db.record) -> find t r.id = None)
      (rest @ promoted)
  in
  List.iter (fun (r : Timer_db.record) -> Js.Dict.set t.by_id r.id r) fresh;
  t.records <- sort_records (t.records @ fresh);
  let* _ =
    if promoted = [] then return true
    else begin
      t.callbacks.log
        ("Kept "
        ^ string_of_int (List.length promoted)
        ^ " set"
        ^ (if List.length promoted = 1 then "" else "s")
        ^ " left unreviewed when the page last closed");
      enqueue t (fun () ->
          write t (List.map (fun r -> Timer_db.Put_record r) promoted))
    end
  in
  t.callbacks.on_change ();
  return ()
