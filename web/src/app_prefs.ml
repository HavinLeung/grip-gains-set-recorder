open Timer_core

type t = { page_zoom : float; keep_awake : bool }

let storage_key = "grip-gains-set-recorder.settings.v1"

(* The stored object (a page size outside 50-150 reads as 85). *)
let stored () = Settings.of_string (Browser.storage_get storage_key)

(* Read once. *)
let current : t option ref = ref None
let listeners : (int * (unit -> unit)) list ref = ref []
let next_listener = ref 0

let get () =
  match !current with
  | Some t -> t
  | None ->
      let s = stored () in
      let t = { page_zoom = s.page_zoom; keep_awake = s.keep_awake } in
      current := Some t;
      t

let subscribe f =
  let id = !next_listener in
  incr next_listener;
  listeners := (id, f) :: !listeners;
  fun () -> listeners := List.filter (fun (i, _) -> i <> id) !listeners

let stamp (s : Settings.t) =
  let t = get () in
  { s with page_zoom = t.page_zoom; keep_awake = t.keep_awake }

(* Scales the whole page (Bluefy has no page zoom of its own). CSS zoom keeps layout and tap
   targets consistent, unlike pinch zoom. *)
let root () = Browser.document_element Browser.document

let apply () =
  let z = Js.Float.toString ((get ()).page_zoom /. 100.) in
  if Browser.style_get (root ()) "zoom" <> z then
    Browser.style_set (root ()) "zoom" z

(* Only the two keys change: every other key of the object is written back as it was read. *)
let set t =
  current := Some t;
  Browser.storage_put storage_key
    (Some (Settings.to_string (stamp (stored ()))));
  apply ();
  List.iter (fun (_, f) -> f ()) !listeners

(* The rounding and limits live with the core's settings. *)
let zoomed f page_zoom =
  (f { Settings.defaults with page_zoom }).Settings.page_zoom

let set_page_zoom x =
  let t = get () in
  set { t with page_zoom = zoomed (fun s -> Settings.set_zoom s x) t.page_zoom }

let step_page_zoom d =
  let t = get () in
  set
    { t with page_zoom = zoomed (fun s -> Settings.step_zoom s d) t.page_zoom }

let set_keep_awake keep_awake = set { (get ()) with keep_awake }

let use () =
  let _, force = React.useReducer (fun n () -> n + 1) 0 in
  React.useEffect0 (fun () -> Some (subscribe (fun () -> force ())));
  get ()
