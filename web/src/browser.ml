(* Hand-written bindings to browser APIs: the one module the rest of the app reaches the
   browser through (the HTTP client is Http, IndexedDB is Idb).

   House style:
   - one abstract type per JS object kind ([document], [element], [characteristic]);
   - globals with a plain [external x : t = "x"];
   - properties with [@@mel.get], methods with [@@mel.send], object first;
   - anything that may be absent is typed [Js.undefined] / [Js.nullable] at the binding and
     converted to [option] immediately, so the rest of the code never sees JS null/undefined;
   - no [%mel.raw], no [Obj.magic].

   No interface file: the externals are the interface, and an .mli would only repeat them. *)

(* --- errors from rejected promises and thrown exceptions --- *)

(* A JS rejection, reduced to what the diagnostic log and messages need. *)
type err = { name : string; message : string }

external err_name : Js.Promise.error -> string Js.Nullable.t = "name"
[@@mel.get]

external err_message : Js.Promise.error -> string Js.Nullable.t = "message"
[@@mel.get]

let err_of_promise_error (e : Js.Promise.error) =
  let get f =
    try Option.value ~default:"" (Js.Nullable.toOption (f e)) with _ -> ""
  in
  { name = get err_name; message = get err_message }

let err_of_exn = function
  | Js.Exn.Error e ->
      {
        name = Option.value ~default:"Error" (Js.Exn.name e);
        message = Option.value ~default:"" (Js.Exn.message e);
      }
  | _ -> { name = "Error"; message = "" }

let ( let* ) p f = Js.Promise.then_ f p
let return = Js.Promise.resolve

(* A promise as a [result]: never rejects. *)
let attempt (p : 'a Js.Promise.t) : ('a, err) result Js.Promise.t =
  Js.Promise.catch
    (fun e -> return (Error (err_of_promise_error e)))
    (Js.Promise.then_ (fun v -> return (Ok v)) p)

(* Calls [f] and turns a synchronous throw into a rejected-as-[Error] result too. *)
let attempt_call (f : unit -> 'a Js.Promise.t) : ('a, err) result Js.Promise.t =
  match f () with
  | p -> attempt p
  | exception e -> return (Error (err_of_exn e))

let sleep ms : unit Js.Promise.t =
  Js.Promise.make (fun ~resolve ~reject:_ ->
      ignore
        (Js.Global.setTimeout
           ~f:(fun () ->
             let unit = () in
             (resolve unit [@u]))
           ms))

(* --- globals --- *)

type window
type document
type element
type dom_event

external window : window = "window"
external document : document = "document"
external now : unit -> float = "now" [@@mel.scope "performance"]
external date_now : unit -> float = "now" [@@mel.scope "Date"]

external device_pixel_ratio : window -> float Js.undefined = "devicePixelRatio"
[@@mel.get]

let dpr () =
  match Js.Undefined.toOption (device_pixel_ratio window) with
  | Some d when d > 0. -> d
  | _ -> 1.

external add_window_listener : window -> string -> (dom_event -> unit) -> unit
  = "addEventListener"
[@@mel.send]

external remove_window_listener :
  window -> string -> (dom_event -> unit) -> unit = "removeEventListener"
[@@mel.send]

external add_doc_listener :
  document -> string -> (dom_event -> unit) -> bool -> unit = "addEventListener"
[@@mel.send]

external remove_doc_listener :
  document -> string -> (dom_event -> unit) -> bool -> unit
  = "removeEventListener"
[@@mel.send]

(* --- light/dark: prefers-color-scheme changes (the canvases read CSS colours when drawn) --- *)

type media_query_list

external match_media : window -> string -> media_query_list = "matchMedia"
[@@mel.send]

external add_mql_listener :
  media_query_list -> string -> (dom_event -> unit) -> unit = "addEventListener"
[@@mel.send]

external remove_mql_listener :
  media_query_list -> string -> (dom_event -> unit) -> unit
  = "removeEventListener"
[@@mel.send]

(* Calls [f] whenever the system switches between light and dark; returns the unsubscribe. *)
let on_color_scheme_change f =
  try
    let mql = match_media window "(prefers-color-scheme: dark)" in
    let h _ = f () in
    add_mql_listener mql "change" h;
    fun () -> try remove_mql_listener mql "change" h with _ -> ()
  with _ -> fun () -> ()

external visibility_state : document -> string = "visibilityState" [@@mel.get]
external hostname : string = "hostname" [@@mel.scope "location"]
external search : string = "search" [@@mel.scope "location"]
external user_agent : string = "userAgent" [@@mel.scope "navigator"]
external on_line : bool = "onLine" [@@mel.scope "navigator"]
external self_ : window = "self" [@@mel.scope "window"]
external top_ : window = "top" [@@mel.scope "window"]
external same_window : window -> window -> bool = "is" [@@mel.scope "Object"]

(* Web Bluetooth only works in a top-level page. *)
let top_level () = try same_window self_ top_ with _ -> false

external event_key : dom_event -> string Js.undefined = "key" [@@mel.get]
external event_target : dom_event -> element = "target" [@@mel.get]
external prevent_default : dom_event -> unit = "preventDefault" [@@mel.send]

(* securitypolicyviolation fields (all strings, possibly empty). *)
external effective_directive : dom_event -> string Js.undefined
  = "effectiveDirective"
[@@mel.get]

external violated_directive : dom_event -> string Js.undefined
  = "violatedDirective"
[@@mel.get]

external blocked_uri : dom_event -> string Js.undefined = "blockedURI"
[@@mel.get]

external source_file : dom_event -> string Js.undefined = "sourceFile"
[@@mel.get]

let undef_str x = Option.value ~default:"" (Js.Undefined.toOption x)

(* --- elements (just enough for the log, the copy fallback, select-all and zoom) --- *)

external create_element : document -> string -> element = "createElement"
[@@mel.send]

external create_text_node : document -> string -> element = "createTextNode"
[@@mel.send]

external body : document -> element = "body" [@@mel.get]
external document_element : document -> element = "documentElement" [@@mel.get]
external append_child : element -> element -> unit = "appendChild" [@@mel.send]
external remove_child : element -> element -> unit = "removeChild" [@@mel.send]

external first_child : element -> element Js.Nullable.t = "firstChild"
[@@mel.get]

external remove : element -> unit = "remove" [@@mel.send]

external set_attribute : element -> string -> string -> unit = "setAttribute"
[@@mel.send]

external get_attribute : element -> string -> string Js.Nullable.t
  = "getAttribute"
[@@mel.send]

external remove_attribute : element -> string -> unit = "removeAttribute"
[@@mel.send]

external set_text_content : element -> string -> unit = "textContent"
[@@mel.set]

external set_inner_text_value : element -> string -> unit = "value" [@@mel.set]
external tag_name : element -> string Js.undefined = "tagName" [@@mel.get]
external input_type : element -> string Js.undefined = "type" [@@mel.get]
external input_value : element -> string Js.undefined = "value" [@@mel.get]

external class_list_contains : element -> string -> bool = "contains"
[@@mel.send] [@@mel.scope "classList"]

external closest : element -> string -> element Js.Nullable.t = "closest"
[@@mel.send]

external select : element -> unit = "select" [@@mel.send]

external set_selection_range : element -> int -> int -> unit
  = "setSelectionRange"
[@@mel.send]

external active_element : document -> element Js.Nullable.t = "activeElement"
[@@mel.get]

external scroll_top : element -> float = "scrollTop" [@@mel.get]
external set_scroll_top : element -> float -> unit = "scrollTop" [@@mel.set]
external scroll_height : element -> float = "scrollHeight" [@@mel.get]
external client_height : element -> float = "clientHeight" [@@mel.get]
external client_width : element -> float = "clientWidth" [@@mel.get]

external style_set : element -> string -> string -> unit = "setProperty"
[@@mel.send] [@@mel.scope "style"]

external style_get : element -> string -> string = "getPropertyValue"
[@@mel.send] [@@mel.scope "style"]

external exec_command : document -> string -> bool = "execCommand" [@@mel.send]
external element_eq : element -> element -> bool = "is" [@@mel.scope "Object"]

let has_class el c = try class_list_contains el c with _ -> false

let is_active el =
  match Js.Nullable.toOption (active_element document) with
  | Some a -> element_eq a el
  | None -> false

(* getComputedStyle(el).getPropertyValue(name) *)
type computed

external get_computed_style : element -> computed = "getComputedStyle"

external property_value : computed -> string -> string = "getPropertyValue"
[@@mel.send]

let css_var el name = String.trim (property_value (get_computed_style el) name)

(* --- localStorage (every access can throw: private mode, blocked storage) --- *)

external ls_get : string -> string Js.Nullable.t = "getItem"
[@@mel.scope "localStorage"]

external ls_set : string -> string -> unit = "setItem"
[@@mel.scope "localStorage"]

external ls_remove : string -> unit = "removeItem" [@@mel.scope "localStorage"]

let storage_get k = try Js.Nullable.toOption (ls_get k) with _ -> None
let storage_set k v = try Ok (ls_set k v) with e -> Error (err_of_exn e)
let storage_remove k = try ls_remove k with _ -> ()

(* Keeps [Some v] under the key, forgets the key for [None]; a refusal is ignored. *)
let storage_put k = function
  | Some v -> ignore (storage_set k v)
  | None -> storage_remove k

(* navigator.storage.persist(): ask the browser not to evict this origin's storage. *)
type storage_manager

external storage_manager : storage_manager Js.undefined = "storage"
[@@mel.scope "navigator"]

external persist : storage_manager -> bool Js.Promise.t = "persist" [@@mel.send]

let request_persistent_storage () =
  match Js.Undefined.toOption storage_manager with
  | Some m -> ( try ignore (attempt (persist m)) with _ -> ())
  | None -> ()

(* --- ids --- *)

external random_uuid : unit -> string = "randomUUID" [@@mel.scope "crypto"]

type uint8_array

external uint8_of_ints : int array -> uint8_array = "Uint8Array" [@@mel.new]
external uint8_create : int -> uint8_array = "Uint8Array" [@@mel.new]

external get_random_values : uint8_array -> uint8_array = "getRandomValues"
[@@mel.scope "crypto"]

external uint8_to_ints : uint8_array -> int array = "from" [@@mel.scope "Array"]

(* The MVP's newUUID: crypto.randomUUID, or a v4 UUID from getRandomValues where it is missing
   (older WebKit outside a secure context). *)
let new_uuid () =
  try random_uuid ()
  with _ ->
    let b = uint8_to_ints (get_random_values (uint8_create 16)) in
    b.(6) <- b.(6) land 15 lor 64;
    b.(8) <- b.(8) land 63 lor 128;
    let hex = "0123456789abcdef" in
    let h = Buffer.create 36 in
    Array.iteri
      (fun i x ->
        if i = 4 || i = 6 || i = 8 || i = 10 then Buffer.add_char h '-';
        Buffer.add_char h hex.[x lsr 4];
        Buffer.add_char h hex.[x land 15])
      b;
    Buffer.contents h

(* --- clipboard --- *)

external clipboard_write : string -> unit Js.Promise.t = "writeText"
[@@mel.scope "navigator", "clipboard"]

(* The MVP's copyText: the async clipboard, else a hidden textarea and execCommand("copy").
   Resolves to whether it worked. *)
let copy_text text : bool Js.Promise.t =
  let fallback () =
    let ta = create_element document "textarea" in
    set_inner_text_value ta text;
    set_attribute ta "readonly" "";
    style_set ta "position" "fixed";
    style_set ta "opacity" "0";
    append_child (body document) ta;
    let ok =
      try
        select ta;
        set_selection_range ta 0 (String.length text);
        exec_command document "copy"
      with _ -> false
    in
    remove ta;
    ok
  in
  match clipboard_write text with
  | p ->
      Js.Promise.catch
        (fun _ -> return (fallback ()))
        (Js.Promise.then_ (fun () -> return true) p)
  | exception _ -> return (fallback ())

(* --- DataView (Bluetooth notifications) --- *)

type data_view

external byte_length : data_view -> int = "byteLength" [@@mel.get]
external get_uint8 : data_view -> int -> int = "getUint8" [@@mel.send]

(* --- Web Bluetooth --- *)

type bluetooth
type bt_device
type gatt_server
type gatt_service
type characteristic

external bluetooth : bluetooth Js.undefined = "bluetooth"
[@@mel.scope "navigator"]

let bluetooth () = try Js.Undefined.toOption bluetooth with _ -> None

type bt_filter = { namePrefix : string option; services : string array option }

type request_options = {
  filters : bt_filter array option;
  acceptAllDevices : bool option;
  optionalServices : string array;
}

external request_device : bluetooth -> request_options -> bt_device Js.Promise.t
  = "requestDevice"
[@@mel.send]

external device_name : bt_device -> string Js.Nullable.t = "name" [@@mel.get]
external device_gatt : bt_device -> gatt_server = "gatt" [@@mel.get]

external add_device_listener :
  bt_device -> string -> (dom_event -> unit) -> unit = "addEventListener"
[@@mel.send]

external remove_device_listener :
  bt_device -> string -> (dom_event -> unit) -> unit = "removeEventListener"
[@@mel.send]

external gatt_connect : gatt_server -> gatt_server Js.Promise.t = "connect"
[@@mel.send]

external gatt_disconnect : gatt_server -> unit = "disconnect" [@@mel.send]
external gatt_connected : gatt_server -> bool = "connected" [@@mel.get]

external get_primary_service :
  gatt_server -> string -> gatt_service Js.Promise.t = "getPrimaryService"
[@@mel.send]

external get_characteristic :
  gatt_service -> string -> characteristic Js.Promise.t = "getCharacteristic"
[@@mel.send]

external start_notifications : characteristic -> characteristic Js.Promise.t
  = "startNotifications"
[@@mel.send]

external stop_notifications : characteristic -> characteristic Js.Promise.t
  = "stopNotifications"
[@@mel.send]

external add_char_listener :
  characteristic -> string -> (dom_event -> unit) -> unit = "addEventListener"
[@@mel.send]

external notification_value : dom_event -> data_view = "value"
[@@mel.get] [@@mel.scope "target"]

external write_with_response :
  characteristic -> uint8_array -> unit Js.Promise.t = "writeValueWithResponse"
[@@mel.send]

external write_value : characteristic -> uint8_array -> unit Js.Promise.t
  = "writeValue"
[@@mel.send]

external has_write_with_response : characteristic -> 'f Js.undefined
  = "writeValueWithResponse"
[@@mel.get]

let gatt_connected_safe d = try gatt_connected (device_gatt d) with _ -> false
let gatt_disconnect_safe d = try gatt_disconnect (device_gatt d) with _ -> ()

(* --- WebAudio --- *)

type audio_context
type audio_node
type audio_param
type audio_buffer
type html_audio

external has_audio_context : window -> 'c Js.undefined = "AudioContext"
[@@mel.get]

external has_webkit_audio_context : window -> 'c Js.undefined
  = "webkitAudioContext"
[@@mel.get]

external new_audio_context : unit -> audio_context = "AudioContext" [@@mel.new]

external new_webkit_audio_context : unit -> audio_context = "webkitAudioContext"
[@@mel.new]

(* new (window.AudioContext || window.webkitAudioContext)() *)
let make_audio_context () =
  if Js.Undefined.toOption (has_audio_context window) <> None then
    new_audio_context ()
  else new_webkit_audio_context ()

external ctx_state : audio_context -> string = "state" [@@mel.get]
external ctx_resume : audio_context -> unit Js.Promise.t = "resume" [@@mel.send]
external ctx_close : audio_context -> unit Js.Promise.t = "close" [@@mel.send]
external ctx_current_time : audio_context -> float = "currentTime" [@@mel.get]

external ctx_destination : audio_context -> audio_node = "destination"
[@@mel.get]

external create_oscillator : audio_context -> audio_node = "createOscillator"
[@@mel.send]

external create_gain : audio_context -> audio_node = "createGain" [@@mel.send]

external create_buffer_source : audio_context -> audio_node
  = "createBufferSource"
[@@mel.send]

external create_buffer : audio_context -> int -> int -> int -> audio_buffer
  = "createBuffer"
[@@mel.send]

external set_source_buffer : audio_node -> audio_buffer -> unit = "buffer"
[@@mel.set]

external set_osc_type : audio_node -> string -> unit = "type" [@@mel.set]
external frequency : audio_node -> audio_param = "frequency" [@@mel.get]
external gain : audio_node -> audio_param = "gain" [@@mel.get]

external set_value_at_time : audio_param -> float -> float -> unit
  = "setValueAtTime"
[@@mel.send]

external linear_ramp : audio_param -> float -> float -> unit
  = "linearRampToValueAtTime"
[@@mel.send]

external connect_node : audio_node -> audio_node -> unit = "connect"
[@@mel.send]

external start_node : audio_node -> float -> unit = "start" [@@mel.send]
external stop_node : audio_node -> float -> unit = "stop" [@@mel.send]

(* navigator.audioSession (Safari 16.4+; Bluefy, being WebKit, may or may not expose it). *)
type audio_session

external audio_session : audio_session Js.undefined = "audioSession"
[@@mel.scope "navigator"]

external session_type : audio_session -> string = "type" [@@mel.get]
external set_session_type : audio_session -> string -> unit = "type" [@@mel.set]

let audio_session () = try Js.Undefined.toOption audio_session with _ -> None

external new_audio : string -> html_audio = "Audio" [@@mel.new]

external audio_set_attribute : html_audio -> string -> string -> unit
  = "setAttribute"
[@@mel.send]

external audio_play : html_audio -> unit Js.Promise.t Js.undefined = "play"
[@@mel.send]

external audio_pause : html_audio -> unit = "pause" [@@mel.send]
external audio_paused : html_audio -> bool = "paused" [@@mel.get]

type blob
type blob_options = { type_ : string [@mel.as "type"] }

external new_blob : uint8_array array -> blob_options -> blob = "Blob"
[@@mel.new]

external create_object_url : blob -> string = "createObjectURL"
[@@mel.scope "URL"]

(* --- Screen Wake Lock --- *)

type wake_lock
type wake_sentinel

external wake_lock : wake_lock Js.undefined = "wakeLock"
[@@mel.scope "navigator"]

external wake_request : wake_lock -> string -> wake_sentinel Js.Promise.t
  = "request"
[@@mel.send]

external sentinel_released : wake_sentinel -> bool = "released" [@@mel.get]

external sentinel_release : wake_sentinel -> unit Js.Promise.t = "release"
[@@mel.send]

external add_sentinel_listener :
  wake_sentinel -> string -> (dom_event -> unit) -> unit = "addEventListener"
[@@mel.send]

let wake_lock () = try Js.Undefined.toOption wake_lock with _ -> None

(* --- canvas 2D --- *)

type canvas
type ctx

external canvas_of_element : Dom.element -> canvas = "%identity"
external get_context : canvas -> string -> ctx = "getContext" [@@mel.send]
external canvas_width : canvas -> int = "width" [@@mel.get]
external canvas_height : canvas -> int = "height" [@@mel.get]
external set_canvas_width : canvas -> int -> unit = "width" [@@mel.set]
external set_canvas_height : canvas -> int -> unit = "height" [@@mel.set]
external canvas_client_width : canvas -> float = "clientWidth" [@@mel.get]
external canvas_client_height : canvas -> float = "clientHeight" [@@mel.get]
external canvas_element : canvas -> element = "%identity"

external set_transform :
  ctx -> float -> float -> float -> float -> float -> float -> unit
  = "setTransform"
[@@mel.send]

external clear_rect : ctx -> float -> float -> float -> float -> unit
  = "clearRect"
[@@mel.send]

external fill_rect : ctx -> float -> float -> float -> float -> unit
  = "fillRect"
[@@mel.send]

external set_fill_style : ctx -> string -> unit = "fillStyle" [@@mel.set]
external set_stroke_style : ctx -> string -> unit = "strokeStyle" [@@mel.set]
external set_line_width : ctx -> float -> unit = "lineWidth" [@@mel.set]
external set_line_join : ctx -> string -> unit = "lineJoin" [@@mel.set]
external set_line_dash : ctx -> float array -> unit = "setLineDash" [@@mel.send]
external set_font : ctx -> string -> unit = "font" [@@mel.set]
external set_text_align : ctx -> string -> unit = "textAlign" [@@mel.set]
external set_text_baseline : ctx -> string -> unit = "textBaseline" [@@mel.set]
external begin_path : ctx -> unit = "beginPath" [@@mel.send]
external move_to : ctx -> float -> float -> unit = "moveTo" [@@mel.send]
external line_to : ctx -> float -> float -> unit = "lineTo" [@@mel.send]
external stroke : ctx -> unit = "stroke" [@@mel.send]
external fill : ctx -> unit = "fill" [@@mel.send]

external arc : ctx -> float -> float -> float -> float -> float -> unit = "arc"
[@@mel.send]

external rect : ctx -> float -> float -> float -> float -> unit = "rect"
[@@mel.send]

external clip : ctx -> unit = "clip" [@@mel.send]
external save : ctx -> unit = "save" [@@mel.send]
external restore : ctx -> unit = "restore" [@@mel.send]

external fill_text : ctx -> string -> float -> float -> unit = "fillText"
[@@mel.send]

external request_animation_frame : (float -> unit) -> int
  = "requestAnimationFrame"

external cancel_animation_frame : int -> unit = "cancelAnimationFrame"

(* --- dynamic script (the simulated Progressor on localhost) --- *)

external set_src : element -> string -> unit = "src" [@@mel.set]
external head : document -> element = "head" [@@mel.get]

(* --- the service worker that keeps the app shell for offline use (web/static/sw.js) --- *)

type service_workers
type registration

external service_worker : service_workers Js.undefined = "serviceWorker"
[@@mel.scope "navigator"]

external sw_register : service_workers -> string -> registration Js.Promise.t
  = "register"
[@@mel.send]

external sw_controller : service_workers -> registration Js.null = "controller"
[@@mel.get]

(* Absent in browsers (or embedded web views) without service workers, and on insecure origins. *)
let service_workers () : service_workers option =
  try Js.Undefined.toOption service_worker with _ -> None

(* Feature-detected; every failure is silent: without a service worker the app simply needs a
   connection to open, as before. *)
let register_service_worker () =
  match service_workers () with
  | None -> ()
  | Some sw -> (
      try
        (* relative: the app may be served from a sub-path *)
        sw_register sw "sw.js"
        |> Js.Promise.then_ (fun _ -> Js.Promise.resolve ())
        |> Js.Promise.catch (fun _ -> Js.Promise.resolve ())
        |> ignore
      with _ -> ())

(* Whether this page is served through the service worker (so a copy of the app is on the device). *)
let offline_copy () =
  match service_workers () with
  | None -> `Unsupported
  | Some sw -> (
      try
        if Js.Null.toOption (sw_controller sw) <> None then `Ready else `Not_yet
      with _ -> `Unsupported)

(* --- forms --- *)

(* [event.target.value] for input/change events. *)
external target_value : React.Event.Form.t -> string = "value"
[@@mel.get] [@@mel.scope "target"]

external encode_uri_component : string -> string = "encodeURIComponent"

(* Removes a key from a dictionary in place (Js.Dict.unsafeDeleteKey only takes string dicts). *)
external dict_delete : 'a Js.Dict.t -> string -> bool = "deleteProperty"
[@@mel.scope "Reflect"]

(* --- React refs --- *)

external el_of_dom : Dom.element -> element = "%identity"
external set_inner_html : element -> string -> unit = "innerHTML" [@@mel.set]
external set_class_name : element -> string -> unit = "className" [@@mel.set]

let ref_el (r : Dom.element Js.Nullable.t React.ref) =
  Option.map el_of_dom (Js.Nullable.toOption r.current)

let ref_canvas (r : Dom.element Js.Nullable.t React.ref) =
  Option.map canvas_of_element (Js.Nullable.toOption r.current)

(* HTML-escape (the MVP's esc) for the few lines built as HTML. *)
let esc s =
  let r re rep s = Js.String.replaceByRe ~regexp:re ~replacement:rep s in
  s
  |> r [%re "/&/g"] "&amp;"
  |> r [%re "/</g"] "&lt;"
  |> r [%re "/>/g"] "&gt;"
  |> r [%re "/\"/g"] "&quot;"

external target_checked : React.Event.Form.t -> bool = "checked"
[@@mel.get] [@@mel.scope "target"]

external class_list_add : element -> string -> unit = "add"
[@@mel.send] [@@mel.scope "classList"]

external class_list_remove : element -> string -> unit = "remove"
[@@mel.send] [@@mel.scope "classList"]

external focus : element -> unit = "focus" [@@mel.send]
external mouse_target : React.Event.Mouse.t -> element = "target" [@@mel.get]
external confirm : string -> bool = "confirm"
