(* The Timer's controller: the I/O shell around Timer_core.Session (the force handler and set
   flow). It owns the session state, the graph's force buffer, the Bluetooth connection,
   audio, the wake lock, the diagnostic log and the set storage (Timer_sets), and it feeds the
   core:

   - every Bluetooth notification as [Packet] (with performance.now());
   - a [Tick] every 100 ms (set timing runs on its own timer, so countdown beeps keep time even
     when the browser slows animation frames);
   - user actions (start / end set, connection changes, settings changes);

   and acts on the events the core returns: samples and grip marks go to the Force_buffer, tones
   to Timer_audio, [Save_set] to the device storage, [Set_ended] opens the review.

   One engine for the page (module-level [current]), outside React. The view subscribes for
   structural changes (connection, phase, reps...) and registers a draw callback that the frame
   loop calls for the per-frame readout and graph.

   "MVP" in comments is the first version of this app, a single HTML page with the same logic
   in JavaScript; the comments marked "MVP:" carry over its hard-won iOS/Bluefy notes. *)

open Timer_core

type conn = Off | Connecting | Live | Reconnecting

(* The red box under the connect button: a lead sentence, an optional error name/message shown
   as code, and extra lines. *)
type notice = { lead : string; code : string option; lines : string list }

type review = {
  set : History.set;
  manual : bool;
  kg : float;
  discard_armed : bool;
}

(* An open set detail sheet: the set's id and, once loaded, the set as recorded. *)
type sheet = {
  sheet_id : string;
  trace : History.set option;
  loading : bool;
  delete_armed : bool;
}

type t = {
  log : Timer_log.t;
  audio : Timer_audio.t;
  sets : Timer_sets.t;
  buffer : Force_buffer.t;
  mutable settings : Settings.t;
  mutable session : Session.t;
  mutable conn : conn;
  mutable device : Browser.bt_device option;
  mutable write_char : Browser.characteristic option;
  mutable notify_char : Browser.characteristic option;
  mutable battery : int option;
  mutable user_disconnect : bool;
  mutable reconnect_tries : int;
  mutable reconnect_loop : bool;
  mutable conn_attempt : int;
  mutable conn_step : string;
  mutable rate_count : int;  (** samples since rate_start *)
  mutable samples_total : int;
  mutable rate_start : float;
  mutable hz : int;
  mutable frozen : float option;
  mutable review : review option;
  mutable last_set : History.set option;
      (** the last set that ended (the idle timer shows its last rep) *)
  mutable sheet : sheet option;
  mutable error : notice option;
  mutable toast : string option;
  mutable toast_timer : Js.Global.timeoutId option;
  mutable discard_timer : Js.Global.timeoutId option;
  mutable wake : Browser.wake_sentinel option;
  wake_logged : string ref;
  mutable listeners : (int * (unit -> unit)) list;
  mutable next_listener : int;
  mutable signature : string;
  mutable draw : (t -> render_t:float -> now:float -> unit) option;
  mutable raf : int option;
  mutable pc_token : string;
  mutable pc_token_error : bool;
  mutable pc_inflight : string list;
  mutable packet_handler : Browser.dom_event -> unit;
  mutable disconnect_handler : Browser.dom_event -> unit;
  mutable set_manual : bool;
      (** the set in progress (or in review) was started in manual mode *)
}

let svc = Protocol.service_uuid
let notify_uuid = Protocol.notify_uuid
let write_uuid = Protocol.write_uuid
let ( let* ) p f = Js.Promise.then_ f p
let return = Js.Promise.resolve

(* --- localStorage keys --- *)

let settings_key = App_prefs.storage_key
let token_key = "grip-gains-set-recorder.powercurve-token.v1"

(* --- subscriptions --- *)

let notify t = List.iter (fun (_, f) -> f ()) t.listeners

let subscribe t f =
  let id = t.next_listener in
  t.next_listener <- id + 1;
  t.listeners <- (id, f) :: t.listeners;
  fun () -> t.listeners <- List.filter (fun (i, _) -> i <> id) t.listeners

let log t msg =
  t.conn_step <- msg;
  Timer_log.step t.log msg

let toast t msg =
  t.toast <- Some msg;
  (match t.toast_timer with Some id -> Js.Global.clearTimeout id | None -> ());
  t.toast_timer <-
    Some
      (Js.Global.setTimeout
         ~f:(fun () ->
           t.toast <- None;
           notify t)
         1800);
  notify t

let show_error t e =
  t.error <- e;
  notify t

let in_set t = Session.in_set t.session
let streaming t = t.conn = Live

(* --- settings (in this browser's localStorage, as a JSON object) ---
   [t.settings] is the core's, with manual mode in its [extra] (Timer_prefs). The page size and
   keep-awake are in the same stored object but belong to App_prefs. *)

let manual t = Timer_prefs.manual t.settings

let save_settings t =
  match
    Browser.storage_set settings_key
      (Settings.to_string (App_prefs.stamp t.settings))
  with
  | Ok () -> ()
  | Error e -> log t ("Couldn't save settings: " ^ e.name ^ ": " ^ e.message)

(* --- wake lock (MVP lines 1127-1151) ---
   MVP: While connected, hold the Screen Wake Lock so the screen doesn't sleep mid-set. The browser
   drops it whenever the page is hidden, so it's re-requested when the page is visible again or
   tapped. (An earlier silent looping video fallback was removed: Bluefy grants the wake lock.) *)

let log_wake t msg = Timer_log.once t.log t.wake_logged msg

let keep_awake t =
  if
    (App_prefs.get ()).keep_awake
    && Browser.visibility_state Browser.document = "visible"
  then
    match Browser.wake_lock () with
    | None ->
        log_wake t
          "Keep awake: this browser has no screen wake lock, so the screen may \
           sleep"
    | Some wl -> (
        match t.wake with
        | Some w when not (Browser.sentinel_released w) ->
            log_wake t "Keep awake: wake lock on"
        | _ ->
            ignore
              (let* r =
                 Browser.attempt_call (fun () ->
                     Browser.wake_request wl "screen")
               in
               (match r with
               | Ok w ->
                   t.wake <- Some w;
                   Browser.add_sentinel_listener w "release" (fun _ ->
                       if t.conn = Live then
                         log_wake t
                           "Keep awake: wake lock released by the browser \
                            (it's requested again on the next tap or when the \
                            page is shown)");
                   log_wake t "Keep awake: wake lock on"
               | Error e ->
                   log_wake t
                     ("Keep awake: wake lock refused (" ^ e.name
                    ^ "), so the screen may sleep"));
               return ()))

let release_awake t =
  (match t.wake with
  | Some w when not (Browser.sentinel_released w) -> (
      try ignore (Browser.attempt (Browser.sentinel_release w)) with _ -> ())
  | _ -> ());
  t.wake <- None;
  log_wake t "Keep awake: off"

(* Whether something is going on that the screen should stay awake for: a connected Progressor,
   or a manual set (no connection holds the lock then, the set does). *)
let wants_awake t = t.conn = Live || (t.set_manual && Session.in_set t.session)

(* --- core events --- *)

let handle t (ev : Session.event) =
  Force_buffer.apply t.buffer ev;
  Timer_audio.play t.audio t.settings (Sound.tones t.settings ev);
  (match ev with
  | Session.Set_ended _ when t.set_manual && t.conn <> Live -> release_awake t
  | _ -> ());
  match ev with
  | Session.Sample _ ->
      t.rate_count <- t.rate_count + 1;
      t.samples_total <- t.samples_total + 1
  | Battery p ->
      t.battery <- Some p;
      notify t
  | Rep_started | Rest_started | Set_complete | Rep_finished _ -> notify t
  | Save_set set ->
      ignore (Timer_sets.save_recording t.sets ~manual:t.set_manual set)
  | Set_ended (Some set) when set.reps <> [] ->
      t.last_set <- Some set;
      t.review <-
        Some
          {
            set;
            manual = t.set_manual;
            kg = set.target_kg;
            discard_armed = false;
          };
      notify t
  | Set_ended _ -> notify t
  | Buffer_cleared | Grip_started _ | Grip_ended _ | Grip_discarded
  | Countdown_beep _ | Off_target_alert _ ->
      ()

let feed t input =
  let s, evs = Session.step t.session input in
  t.session <- s;
  List.iter (handle t) evs

(* While a set is in progress, what shapes it (target, timings, thresholds, manual mode) stays
   as it is: the panels disable those controls, and this holds for every other path too. *)
let change t (s : Settings.t) =
  (* lbs only: a stored "units": "kg" is ignored, and written back as "lbs". *)
  let s = { s with units = Settings.Lbs } in
  let s =
    if in_set t then
      Timer_prefs.with_manual
        (Settings.keep_set_fields ~current:t.settings s)
        (manual t)
    else s
  in
  t.settings <- s;
  save_settings t;
  feed t (Session.Settings_changed t.settings);
  notify t

(* --- Bluetooth (MVP lines 973-1125) --- *)

let send_cmd t cmd : unit Js.Promise.t =
  match t.write_char with
  | None -> return ()
  | Some c ->
      let data = Browser.uint8_of_ints [| Protocol.command_byte cmd |] in
      if Js.Undefined.toOption (Browser.has_write_with_response c) <> None then
        Browser.write_with_response c data
      else Browser.write_value c data

(* MVP: Each connection step gets a time limit and a log line, so a hang names the step it
   stopped at. *)
let step t label ?(ms = 10000) (f : unit -> 'a Js.Promise.t) :
    ('a, Browser.err) result Js.Promise.t =
  log t (label ^ {js|…|js});
  let attempt_no = t.conn_attempt in
  Js.Promise.make (fun ~resolve ~reject:_ ->
      let settled = ref false in
      let finish v =
        if not !settled then begin
          settled := true;
          resolve v [@u]
        end
      in
      let timer =
        Js.Global.setTimeout
          ~f:(fun () ->
            finish
              (Error
                 {
                   Browser.name = "TimeoutError";
                   message =
                     "No answer after "
                     ^ string_of_int (ms / 1000)
                     ^ "s at: " ^ label;
                 }))
          ms
      in
      ignore
        (let* r = Browser.attempt_call f in
         Js.Global.clearTimeout timer;
         (match r with
         | Ok v ->
             if attempt_no <> t.conn_attempt then
               finish
                 (Error { Browser.name = "AbortError"; message = "Cancelled" })
             else if not !settled then begin
               log t (label ^ {js| ✓|js});
               finish (Ok v)
             end
         | Error e -> finish (Error e));
         return ()))

let ( let*? ) p f =
  Js.Promise.then_ (function Ok v -> f v | Error e -> return (Error e)) p

let device_label d =
  match Js.Nullable.toOption (Browser.device_name d) with
  | Some n -> n
  | None -> "unnamed device"

let rec open_gatt t ~fresh : (unit, Browser.err) result Js.Promise.t =
  match t.device with
  | None ->
      return (Error { Browser.name = "NotFoundError"; message = "No device" })
  | Some d ->
      t.conn_attempt <- t.conn_attempt + 1;
      let attempt_no = t.conn_attempt in
      log t
        (if fresh then {js|Picked “|js} ^ device_label d ^ {js|”|js}
         else {js|Reconnecting…|js});
      let gatt = Browser.device_gatt d in
      let* r =
        step t "Connect to device" ~ms:12000 (fun () ->
            Browser.gatt_connect gatt)
      in
      let* r =
        match r with
        | Error e when e.name = "TimeoutError" && attempt_no = t.conn_attempt ->
            Browser.gatt_disconnect_safe d;
            step t "Connect to device (retry)" ~ms:12000 (fun () ->
                Browser.gatt_connect gatt)
        | r -> return r
      in
      let*? server = return r in
      let*? service =
        step t "Find Progressor service" (fun () ->
            Browser.get_primary_service server svc)
      in
      let*? nc =
        step t "Open data channel" (fun () ->
            Browser.get_characteristic service notify_uuid)
      in
      t.notify_char <- Some nc;
      let*? wc =
        step t "Open command channel" (fun () ->
            Browser.get_characteristic service write_uuid)
      in
      t.write_char <- Some wc;
      (* The same handler every time: browsers hand back the same characteristic object after a
         reconnect, and addEventListener ignores a listener that is already registered, so packets
         are never processed twice. *)
      Browser.add_char_listener nc "characteristicvaluechanged" t.packet_handler;
      let*? _ =
        step t "Turn on notifications" (fun () ->
            Browser.start_notifications nc)
      in
      (* Connected: on a fresh pairing the core clears the graph and the clock; on a reconnect it
         only re-anchors the clock. Then it expects the battery reply. *)
      feed t (Session.Connected { now = Browser.now (); fresh });
      (* A reconnect only counts once the Progressor is measuring again: until then it shows
         as reconnecting and the attempt counts toward the limit (it used to read "Connected"
         and start the count again while "Start measuring" kept failing). *)
      let live () =
        t.conn <- Live;
        t.reconnect_tries <- 0;
        notify t
      in
      if fresh then live ();
      let* b =
        step t "Ask for battery level" ~ms:5000 (fun () ->
            send_cmd t Protocol.Battery)
      in
      (match b with
      | Error e -> log t ("Battery query failed (" ^ e.name ^ "), continuing")
      | Ok () -> ());
      let*? () =
        step t "Start measuring" ~ms:8000 (fun () ->
            send_cmd t Protocol.Start_measuring)
      in
      if not fresh then live ();
      let before = t.samples_total in
      ignore
        (Js.Global.setTimeout
           ~f:(fun () ->
             if attempt_no = t.conn_attempt then
               log t
                 (if
                    t.samples_total > before || Force_buffer.length t.buffer > 0
                  then {js|Readings arriving ✓|js}
                  else "No readings yet after 3s"))
           3000);
      keep_awake t;
      return (Ok ())

and on_disconnected t =
  if t.conn = Connecting then ()
    (* the connect attempt reports its own failure *)
  else begin
    t.write_char <- None;
    t.notify_char <- None;
    if t.user_disconnect || t.device = None then begin
      t.conn <- Off;
      notify t
    end
    else if t.reconnect_loop then () (* already trying *)
    else begin
      log t "Device disconnected";
      t.conn <- Reconnecting;
      t.reconnect_loop <- true;
      notify t;
      let finish () =
        t.reconnect_loop <- false;
        if not t.user_disconnect then begin
          t.conn <- Off;
          release_awake t;
          show_error t
            (Some
               {
                 lead = "Lost the Progressor. Connect again to pair it.";
                 code = None;
                 lines = [];
               })
        end;
        return ()
      in
      let rec loop () =
        if t.user_disconnect || t.reconnect_tries >= 6 then finish ()
        else begin
          t.reconnect_tries <- t.reconnect_tries + 1;
          let* () = Browser.sleep (min (3000 * t.reconnect_tries) 15000) in
          if t.user_disconnect then finish ()
          else
            let* r = open_gatt t ~fresh:false in
            match r with
            | Ok () ->
                t.reconnect_loop <- false;
                return ()
            | Error _ -> loop ()
        end
      in
      ignore (loop ())
    end
  end

let connect_failed t (e : Browser.err) =
  if t.user_disconnect then ()
  else begin
    let was_live = t.conn = Live in
    t.conn <- Off;
    let cancelled =
      e.name = "NotFoundError" && Js.Re.test ~str:e.message [%re "/cancel/i"]
    in
    if cancelled then begin
      log t "Device list closed";
      notify t
    end
    else begin
      log t ("Failed: " ^ e.name ^ ": " ^ e.message);
      t.user_disconnect <- true;
      (match t.device with
      | Some d when Browser.gatt_connected_safe d ->
          Browser.gatt_disconnect_safe d
      | _ -> ());
      show_error t
        (Some
           {
             lead =
               "Couldn't "
               ^ (if was_live then "start measuring" else "connect")
               ^ ":";
             code = Some (e.name ^ ": " ^ e.message);
             lines =
               (if e.name = "TimeoutError" then
                  [
                    "If the Grip Gains Companion app or another app is open, \
                     force-quit it: the Progressor accepts only one connection \
                     at a time. Then connect again.";
                  ]
                else [])
               @ [ {js|Details are under Settings → Diagnostic logs.|js} ];
           })
    end
  end

let connect t ~accept_all =
  Timer_audio.unlock t.audio t.settings;
  t.error <- None;
  match Browser.bluetooth () with
  | _ when manual t -> () (* manual mode doesn't use the Progressor *)
  | None ->
      show_error t
        (Some
           {
             lead =
               "This browser has no Web Bluetooth. Open this page in Chrome or \
                Edge on a computer or Android, or in Bluefy on iPhone.";
             code = None;
             lines = [];
           })
  | Some bt ->
      t.user_disconnect <- false;
      t.conn <- Connecting;
      t.conn_attempt <- t.conn_attempt + 1;
      log t {js|Opened the Bluetooth device list. Waiting for a pick…|js};
      t.conn_step <- "Choose your Progressor in the Bluetooth list.";
      notify t;
      let opts : Browser.request_options =
        if accept_all then
          {
            filters = None;
            acceptAllDevices = Some true;
            optionalServices = [| svc |];
          }
        else
          {
            filters =
              Some
                [|
                  { namePrefix = Some "Progressor"; services = None };
                  { namePrefix = None; services = Some [| svc |] };
                |];
            acceptAllDevices = None;
            optionalServices = [| svc |];
          }
      in
      ignore
        (let* r =
           Browser.attempt_call (fun () -> Browser.request_device bt opts)
         in
         match r with
         | Error e ->
             connect_failed t e;
             return ()
         | Ok d ->
             (match t.device with
             | Some old ->
                 Browser.remove_device_listener old "gattserverdisconnected"
                   t.disconnect_handler
             | None -> ());
             t.device <- Some d;
             Browser.add_device_listener d "gattserverdisconnected"
               t.disconnect_handler;
             let* r = open_gatt t ~fresh:true in
             (match r with Ok () -> () | Error e -> connect_failed t e);
             notify t;
             return ())

let disconnect t =
  t.user_disconnect <- true;
  if t.conn = Connecting && t.device <> None then log t "Cancelled";
  t.conn_attempt <- t.conn_attempt + 1;
  let* () =
    match t.notify_char with
    | Some c ->
        Js.Promise.then_
          (fun _ -> return ())
          (Browser.attempt_call (fun () -> Browser.stop_notifications c))
    | None -> return ()
  in
  (match t.device with
  | Some d when Browser.gatt_connected_safe d -> Browser.gatt_disconnect_safe d
  | _ -> ());
  t.conn <- Off;
  release_awake t;
  if in_set t then feed t (Session.Disconnected { now = Browser.now () });
  notify t;
  return ()

(* Tare: zero the Progressor. Power off: shut it down, then disconnect. *)
let tare t =
  ignore
    (let* r = Browser.attempt_call (fun () -> send_cmd t Protocol.Tare) in
     (match r with
     | Ok () -> toast t "Tared"
     | Error e ->
         toast t "Tare failed";
         log t ("Tare failed: " ^ e.name));
     return ())

let power_off t =
  ignore
    (let* _ = Browser.attempt_call (fun () -> send_cmd t Protocol.Shutdown) in
     disconnect t)

let gripper_name = Grippers.name

(* --- PowerCurve (MVP lines 1504-1584) ---
   MVP: Same request as PowerCurve's Grip Gains import helper (userscript v3.4.0), sent straight
   from the browser to PowerCurve. The token is kept only in this browser. *)

let pc_valid t = History.valid_pc_token t.pc_token

let set_pc_token t v =
  let v = String.trim v in
  if v <> "" && not (History.valid_pc_token v) then begin
    t.pc_token_error <- true;
    notify t
  end
  else begin
    t.pc_token_error <- false;
    t.pc_token <- v;
    if v = "" then Browser.storage_remove token_key
    else ignore (Browser.storage_set token_key v);
    toast t
      (if v <> "" then "PowerCurve token saved" else "PowerCurve token removed")
  end

let json_of_js (j : Js.Json.t) =
  (* A reply of `null` (or anything not an object) reads as {}: the upload then fails with "a reply
     this page doesn't recognise" instead of staying "uploading" for ever, as the MVP could. *)
  match Json.parse (Js.Json.stringify j) with
  | Ok (Json.Object _ as o) -> o
  | _ -> Json.Object []

(* A set whose upload was cut off (page closed mid-request) reads as a failed upload that can
   be retried, never as "uploading" for ever (History.pc_shown). *)
let pc_in_flight t id = List.mem id t.pc_inflight

let pc_state t id (s : History.set) =
  History.pc_shown s.pc ~in_flight:(pc_in_flight t id)

let rec upload_pc t id (set : History.set) =
  if not (pc_valid t) then toast t "Add your PowerCurve token in Settings first"
  else if List.mem id t.pc_inflight then ()
  else begin
    (* sets saved before uploads existed *)
    let set = History.with_session_id set (Browser.new_uuid ()) in
    let set = { set with pc = Some History.Uploading } in
    t.pc_inflight <- id :: t.pc_inflight;
    update_trace t id set;
    let payload =
      History.pc_payload
        { set with gripper = String.lowercase_ascii (gripper_name set.gripper) }
        ~session_id:(Option.value ~default:"" set.source_session_id)
    in
    ignore
      (let* r =
         Http.send "POST" History.pc_url ~credentials:`Omit ~timeout_ms:30_000.
           ~body:(Json.to_string payload)
           ~headers:
             [
               ("Authorization", "Bearer " ^ t.pc_token);
               ("Content-Type", "application/json");
               ("Accept", "application/json");
               ("X-Powercurve-Import-Helper-Version", History.pc_helper_version);
             ]
       in
       let status, body =
         match r with
         | Error e ->
             log t
               ("PowerCurve upload: couldn't reach PowerCurve (" ^ e.name ^ ": "
              ^ e.message ^ ")");
             (None, Json.Object [])
         | Ok { status; json } ->
             log t ("PowerCurve upload: HTTP " ^ string_of_int status);
             (Some status, json_of_js (Option.value json ~default:Js.Json.null))
       in
       let pc =
         History.pc_result ~status ~body
           ~at:(Js.Date.toISOString (Js.Date.make ()))
       in
       let set = { set with pc = Some pc } in
       t.pc_inflight <- List.filter (fun x -> x <> id) t.pc_inflight;
       update_trace t id set;
       (match pc with
       | History.Uploaded { result; _ } -> (
           toast t
             (if result = "already_uploaded" then "Already in PowerCurve"
              else "Uploaded to PowerCurve");
           match t.error with
           | Some { lead; _ }
             when String.length lead >= 24
                  && String.sub lead 0 24 = "PowerCurve upload failed" ->
               show_error t None
           | _ -> ())
       | History.Failed { message; _ } ->
           show_error t
             (Some
                {
                  lead =
                    "PowerCurve upload failed: " ^ message
                    ^ " Retry from the set in History.";
                  code = None;
                  lines = [];
                })
       | _ -> ());
       return ())
  end

and update_trace t id set =
  ignore (Timer_sets.update_trace t.sets id set);
  (match t.sheet with
  | Some sh when sh.sheet_id = id ->
      t.sheet <- Some { sh with trace = Some set }
  | _ -> ());
  notify t

(* --- the end-of-set review (MVP lines 1586-1648) ---
   MVP: Reps are saved as they finish (so closing the page mid-set loses nothing); this step then
   confirms, corrects or removes the set. If the page closes during review, the set stays saved. *)

let review_step t = Settings.target_step t.settings

let review_set_kg t kg =
  match t.review with
  | Some r ->
      t.review <- Some { r with kg };
      notify t
  | None -> ()

(* The −/+ buttons: round the shown weight to the step, then move one step. *)
let review_nudge t d =
  match t.review with
  | Some r ->
      let st = review_step t in
      let shown = Settings.to_display t.settings r.kg in
      let v =
        Float.max st
          ((Jsnum.round (shown /. st) *. st) +. (float_of_int d *. st))
      in
      review_set_kg t (Settings.of_display t.settings v)
  | None -> ()

let close_review t =
  t.review <- None;
  (match t.discard_timer with
  | Some id -> Js.Global.clearTimeout id
  | None -> ());
  notify t

let review_save t =
  match t.review with
  | None -> ()
  | Some r ->
      let set = History.relog r.set r.kg in
      let id = Timer_summary.id_of_set set in
      close_review t;
      ignore
        (let* ok = Timer_sets.confirm t.sets ~manual:r.manual set in
         toast t
           (if ok then "Set saved"
            else "Set kept for this visit only (this browser won't store it)");
         if t.settings.pc_auto && pc_valid t then upload_pc t id set;
         return ())

let review_discard t =
  match t.review with
  | None -> ()
  | Some r when not r.discard_armed ->
      t.review <- Some { r with discard_armed = true };
      t.discard_timer <-
        Some
          (Js.Global.setTimeout
             ~f:(fun () ->
               match t.review with
               | Some r ->
                   t.review <- Some { r with discard_armed = false };
                   notify t
               | None -> ())
             3000);
      notify t
  | Some r ->
      ignore (Timer_sets.discard t.sets r.set);
      close_review t;
      toast t "Set discarded"

(* --- set detail sheet --- *)

let open_sheet t id =
  t.sheet <-
    Some { sheet_id = id; trace = None; loading = true; delete_armed = false };
  notify t;
  ignore
    (let* tr = Timer_sets.trace t.sets id in
     (match t.sheet with
     | Some sh when sh.sheet_id = id ->
         t.sheet <- Some { sh with trace = tr; loading = false }
     | _ -> ());
     notify t;
     return ())

let close_sheet t =
  t.sheet <- None;
  notify t

let delete_set t id =
  ignore
    (let* () = Timer_sets.delete t.sets id in
     (match t.sheet with
     | Some sh when sh.sheet_id = id -> t.sheet <- None
     | _ -> ());
     toast t "Set deleted";
     return ())

let sheet_delete t =
  match t.sheet with
  | Some sh when not sh.delete_armed ->
      t.sheet <- Some { sh with delete_armed = true };
      ignore
        (Js.Global.setTimeout
           ~f:(fun () ->
             match t.sheet with
             | Some s when s.sheet_id = sh.sheet_id ->
                 t.sheet <- Some { s with delete_armed = false };
                 notify t
             | _ -> ())
           3000);
      notify t
  | Some sh -> delete_set t sh.sheet_id
  | None -> ()

let start_set t =
  Timer_audio.unlock t.audio t.settings;
  if not (in_set t) then begin
    t.set_manual <- manual t;
    (* no connection holds the wake lock in manual mode: the set does, until it ends *)
    if t.set_manual then keep_awake t
  end;
  feed t
    (Session.Start_set
       {
         now = Browser.now ();
         wall_ms = Browser.date_now ();
         session_uuid = Browser.new_uuid ();
       });
  notify t

let end_set t =
  feed t (Session.End_set { now = Browser.now () });
  notify t

(* "End rep": ends the current rep now, kept and timed to the tap (Session.End_rep). The core
   ignores it outside a rep (countdown, rest). In manual mode, where every rep ends this way, a
   tap in the first second of a rep is ignored too: the second tap of a double tap (or a tap
   just as a short rest ran out) never ends the next rep. *)
let manual_min_rep_s = 1.

let end_rep t =
  let now = Browser.now () in
  if
    not
      (t.set_manual
      && t.session.phase = Session.Rep
      && Session.rep_elapsed t.session ~now < manual_min_rep_s)
  then begin
    feed t (Session.End_rep { now });
    notify t
  end

(* Turning manual mode on (outside a set): a connected Progressor is disconnected, since manual
   mode hides the connection and would otherwise ignore it while it kept the screen awake and
   reconnected in the background. Turning it off changes nothing else: connect as usual. *)
let set_manual t on =
  if (not (in_set t)) && on <> manual t then begin
    change t (Timer_prefs.with_manual t.settings on);
    if on then begin
      t.error <- None;
      if t.conn <> Off then begin
        log t "Manual mode on: disconnecting the Progressor";
        ignore (disconnect t)
      end
      else if t.wake <> None then release_awake t
    end;
    notify t
  end

(* What the structural UI shows; when it changes, the view re-renders. The numbers that move
   every frame (force, timer, deviation) and the sample rate, which wobbles every second, are
   drawn by the frame loop instead. *)
let signature t =
  let s = t.session in
  String.concat "|"
    [
      (match t.conn with
      | Off -> "off"
      | Connecting -> "connecting"
      | Live -> "live"
      | Reconnecting -> "re");
      (match s.phase with
      | Off -> "o"
      | Countdown -> "c"
      | Rep -> "r"
      | Rest -> "s");
      (match s.handler with Idle -> "i" | Repping -> "p" | Weighing -> "w");
      string_of_bool s.armed;
      string_of_bool s.holding;
      (match s.measured with
      | Some m -> Settings.format ~digits:1 t.settings m
      | None -> "-");
      string_of_int (Session.reps_done s);
      (match t.battery with Some b -> string_of_int b | None -> "-");
      string_of_bool (manual t);
    ]

let on_tick t =
  let now = Browser.now () in
  feed t (Session.Tick { now });
  if now -. t.rate_start >= 1000. then begin
    t.hz <-
      int_of_float
        (Jsnum.round
           (float_of_int t.rate_count *. 1000. /. (now -. t.rate_start)));
    t.rate_count <- 0;
    t.rate_start <- now
  end;
  let sg = signature t in
  if sg <> t.signature then begin
    t.signature <- sg;
    notify t
  end

(* --- frame loop: while a view is mounted --- *)

let rec frame t _ =
  match t.draw with
  | None -> t.raf <- None
  | Some draw ->
      let now = Browser.now () in
      (* MVP: While streaming, draw renderDelayMs behind the clock; otherwise hold the last frame. *)
      let render_t, frozen =
        Graph.render_time ~streaming:(streaming t) ~now
          ~render_delay_ms:t.settings.render_delay_ms ~frozen:t.frozen
          ~last_sample:(Force_buffer.last_time t.buffer)
      in
      t.frozen <- frozen;
      (try draw t ~render_t ~now
       with e -> Js.Console.error2 "Timer draw failed" e);
      t.raf <- Some (Browser.request_animation_frame (frame t))

let attach_view t draw =
  t.draw <- Some draw;
  if t.raf = None then t.raf <- Some (Browser.request_animation_frame (frame t))

let detach_view t =
  t.draw <- None;
  match t.raf with
  | Some id ->
      Browser.cancel_animation_frame id;
      t.raf <- None
  | None -> ()

(* --- controls --- *)

(* Sounds from the settings panel (the test tone, the volume preview): iOS only lets audio start
   from a tap, so every tap that should be heard unlocks it first. *)
let unlock_audio t = Timer_audio.unlock t.audio t.settings
let play t tones = Timer_audio.play t.audio t.settings tones

let set_planned_reps t n =
  if not (in_set t) then change t (Settings.set_planned_reps t.settings n)

let pick_hand t ~gripper ~side =
  if not (in_set t) then change t { t.settings with gripper; side }

(* The target: typed, −/+, or the measured load. *)
let set_target t kg = change t { t.settings with target = kg }

(* --- creation and lifetime --- *)

(* A notification's bytes, as the core reads them. *)
let view_of_data_view dv : Protocol.view =
  {
    length = Browser.byte_length dv;
    get_u8 = (fun i -> Browser.get_uint8 dv i);
  }

let select_number_inputs_within_timer el =
  Option.is_some (Js.Nullable.toOption (Browser.closest el ".timer-tab"))
  && (Browser.undef_str (Browser.input_type el) = "number"
     || Browser.has_class el "token")
  && Browser.undef_str (Browser.tag_name el) = "INPUT"

let install_document_listeners t =
  let add name f capture =
    Browser.add_doc_listener Browser.document name f capture
  in
  (* MVP: Any tap or key press re-unlocks audio, e.g. after the screen was locked. *)
  let unlock _ =
    if Timer_audio.needs_unlock t.audio then
      Timer_audio.unlock t.audio t.settings
  in
  add "pointerdown" unlock true;
  add "keydown" unlock true;
  add "pointerdown"
    (fun _ ->
      if wants_awake t && (App_prefs.get ()).keep_awake then
        match t.wake with
        | Some w when not (Browser.sentinel_released w) -> ()
        | _ -> keep_awake t)
    true;
  add "visibilitychange"
    (fun _ ->
      if Browser.visibility_state Browser.document = "visible" && wants_awake t
      then keep_awake t)
    false;
  (* MVP: If the page is served with a Content Security Policy, log anything it blocks, so a
     broken feature (e.g. a browser's own injected code) is easy to diagnose. *)
  add "securitypolicyviolation"
    (fun e ->
      let d = Browser.undef_str (Browser.effective_directive e) in
      let d =
        if d = "" then Browser.undef_str (Browser.violated_directive e) else d
      in
      let b = Browser.undef_str (Browser.blocked_uri e) in
      let f = Browser.undef_str (Browser.source_file e) in
      log t
        ("Blocked by the site's security policy: " ^ d ^ {js| → |js}
        ^ (if b = "" then "inline" else b)
        ^ if f = "" then "" else " (from " ^ f ^ ")"))
    false;
  (* MVP: Tapping a number box (weight, reps, page size, settings) or the token box selects its
     whole value, so typing replaces it. iOS places the caret on the tap that follows focus, which
     undoes a select() made on focus, so select again on that first click too.
     Both later selects (the deferred one and the click's) only happen while the box still holds
     what it held at focus: once something has been typed, selecting it again would make the
     next key replace it ("30" typed quickly became "0", which the target then refused). *)
  let select_all el = try Browser.select el with _ -> () in
  let value el = Browser.undef_str (Browser.input_value el) in
  let at_focus = "data-select-on-click" in
  let untouched el =
    Js.Nullable.toOption (Browser.get_attribute el at_focus) = Some (value el)
  in
  add "focusin"
    (fun e ->
      let el = Browser.event_target e in
      if select_number_inputs_within_timer el then begin
        Browser.set_attribute el at_focus (value el);
        select_all el;
        ignore
          (Js.Global.setTimeout
             ~f:(fun () ->
               if Browser.is_active el && untouched el then select_all el)
             0)
      end)
    false;
  add "click"
    (fun e ->
      let el = Browser.event_target e in
      if select_number_inputs_within_timer el && untouched el then begin
        Browser.remove_attribute el at_focus;
        select_all el
      end)
    false;
  add "focusout"
    (fun e ->
      let el = Browser.event_target e in
      if select_number_inputs_within_timer el then
        Browser.remove_attribute el "data-select-on-click")
    false

(* The simulated Progressor, for trying the app without the device: ?sim=1. *)
let load_simulator t =
  if Js.Re.test ~str:Browser.search [%re "/[?&]sim=1(&|$)/"] then begin
    let el = Browser.create_element Browser.document "script" in
    Browser.set_src el "sim-progressor.js";
    Browser.append_child (Browser.head Browser.document) el;
    log t "Simulated Progressor (?sim=1)"
  end

let create () =
  let stored = Settings.of_string (Browser.storage_get settings_key) in
  let settings = { stored with units = Settings.Lbs } in
  let lg = Timer_log.create () in
  (* The set storage reports back to the engine that owns it, which doesn't exist yet. *)
  let self = ref None in
  let me f = Option.iter f !self in
  let sets =
    Timer_sets.create
      {
        log = (fun m -> me (fun t -> log t m));
        on_change = (fun () -> me notify);
        on_store_error =
          (fun () ->
            me (fun t ->
                show_error t
                  (Some
                     {
                       lead =
                         "This browser didn't let the page store the set on \
                          this device (its storage may be full). It's kept \
                          only until you close the page.";
                       code = None;
                       lines = [ "Copy its JSON from History to keep a copy." ];
                     })));
      }
  in
  let rec t =
    {
      log = lg;
      audio = Timer_audio.create ~log:(fun m -> Timer_log.step lg m);
      sets;
      buffer = Force_buffer.create ();
      settings;
      session = Session.init settings;
      conn = Off;
      device = None;
      write_char = None;
      notify_char = None;
      battery = None;
      user_disconnect = false;
      reconnect_tries = 0;
      reconnect_loop = false;
      conn_attempt = 0;
      conn_step = "";
      rate_count = 0;
      samples_total = 0;
      rate_start = Browser.now ();
      hz = 0;
      frozen = None;
      review = None;
      last_set = None;
      sheet = None;
      error = None;
      toast = None;
      toast_timer = None;
      discard_timer = None;
      wake = None;
      wake_logged = ref "";
      listeners = [];
      next_listener = 0;
      signature = "";
      draw = None;
      raf = None;
      pc_token = Option.value ~default:"" (Browser.storage_get token_key);
      pc_token_error = false;
      pc_inflight = [];
      packet_handler =
        (fun ev ->
          (* manual mode disconnects the Progressor; a notification still in flight is ignored *)
          if not (manual t) then
            feed t
              (Session.Packet
                 {
                   now = Browser.now ();
                   view = view_of_data_view (Browser.notification_value ev);
                 }));
      disconnect_handler = (fun _ -> on_disconnected t);
      set_manual = false;
    }
  in
  self := Some t;
  (* lbs only: an old stored "units": "kg" is written back as "lbs" straight away. *)
  if stored.units <> Settings.Lbs then save_settings t;
  (* Keep-awake (App_prefs): follow it when it changes. *)
  let (_unsubscribe : unit -> unit) =
    App_prefs.subscribe (fun () ->
        if not (App_prefs.get ()).keep_awake then release_awake t
        else if wants_awake t then keep_awake t)
  in
  install_document_listeners t;
  Browser.request_persistent_storage ();
  let (_tick : Js.Global.intervalId) =
    Js.Global.setInterval ~f:(fun () -> on_tick t) 100
  in
  load_simulator t;
  ignore (Timer_sets.start t.sets);
  log t
    ({js|Page opened · Web Bluetooth |js}
    ^
    match Browser.bluetooth () with
    | Some _ -> "available"
    | None -> "not available");
  t

(* The page's engine, made on first use and kept for the whole visit. *)
let current : t option ref = ref None

let get () =
  match !current with
  | Some t -> t
  | None ->
      let t = create () in
      current := Some t;
      t

(* --- reading the state (the type is abstract outside this file) --- *)

let settings t = t.settings
let session t = t.session
let conn t = t.conn
let conn_step t = t.conn_step
let battery t = t.battery
let hz t = if streaming t then t.hz else 0
let buffer t = t.buffer
let review t = t.review
let sheet t = t.sheet
let error t = t.error
let current_toast t = t.toast
let last_set t = t.last_set
let set_is_manual t = t.set_manual
let sets t = t.sets
let diagnostics t = t.log
let pc_token t = t.pc_token
let pc_token_error t = t.pc_token_error
