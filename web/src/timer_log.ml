(* The diagnostic log (the MVP's logStep / flushLog, lines 987-1027).

   Each connection step gets a time limit and a log line, so a hang names the step it stopped at.
   One diagnostic log for the whole visit: every line timestamped, all kept until the log reaches
   [max_bytes] of text, then the oldest lines go first. Lines are appended to the page one at a
   time (not re-rendered), so a long log stays cheap: the view attaches a <pre> and this module
   appends one text node per line, every 50 ms at most. *)

let max_bytes = 10 * 1024 * 1024

type t = {
  mutable lines : string array;  (** a queue: [lines.(first) ..] *)
  mutable first : int;
  mutable count : int;
  mutable bytes : int;
  mutable dropped : int;
  mutable last : string;  (** the MVP's connStep: the latest message *)
  mutable pending : string list;  (** newest first, not yet on the page *)
  mutable pending_drop : int;
  mutable flush_timer : Js.Global.timeoutId option;
  mutable view : (Browser.element * (string -> unit)) option;
      (** the <pre>, and the count line setter *)
}

let create () =
  {
    lines = Array.make 64 "";
    first = 0;
    count = 0;
    bytes = 0;
    dropped = 0;
    last = "";
    pending = [];
    pending_drop = 0;
    flush_timer = None;
    view = None;
  }

let pad n w =
  let s = string_of_int n in
  if String.length s >= w then s else String.make (w - String.length s) '0' ^ s

(* ISO 8601 local time with milliseconds and UTC offset, e.g. 2026-11-22T22:55:00.123-05:00 *)
let stamp () =
  let d = Js.Date.make () in
  let i f = int_of_float (f d) in
  let off = -i Js.Date.getTimezoneOffset in
  let a = abs off in
  string_of_int (i Js.Date.getFullYear)
  ^ "-"
  ^ pad (i Js.Date.getMonth + 1) 2
  ^ "-"
  ^ pad (i Js.Date.getDate) 2
  ^ "T"
  ^ pad (i Js.Date.getHours) 2
  ^ ":"
  ^ pad (i Js.Date.getMinutes) 2
  ^ ":"
  ^ pad (i Js.Date.getSeconds) 2
  ^ "."
  ^ pad (i Js.Date.getMilliseconds) 3
  ^ (if off >= 0 then "+" else "-")
  ^ pad (a / 60) 2
  ^ ":"
  ^ pad (a mod 60) 2

(* log text is almost all ASCII, so characters ≈ bytes *)
let line_bytes l = String.length l + 1
let nth t i = t.lines.((t.first + i) mod Array.length t.lines)

let push_line t line =
  if t.count = Array.length t.lines then begin
    let bigger = Array.make (2 * t.count) "" in
    for i = 0 to t.count - 1 do
      bigger.(i) <- nth t i
    done;
    t.lines <- bigger;
    t.first <- 0
  end;
  t.lines.((t.first + t.count) mod Array.length t.lines) <- line;
  t.count <- t.count + 1

let all_lines t = List.init t.count (nth t)

let size_text t =
  let kb = float_of_int t.bytes /. 1024. in
  if kb < 1024. then
    string_of_int (max 1 (int_of_float (Float.round kb))) ^ " KB"
  else Js.Float.toFixed ~digits:1 (kb /. 1024.) ^ " MB"

let count_text t =
  string_of_int t.count ^ " line"
  ^ (if t.count = 1 then "" else "s")
  ^ {js| · |js} ^ size_text t ^ " of 10 MB"
  ^
  if t.dropped > 0 then
    {js| · |js} ^ string_of_int t.dropped ^ " oldest line"
    ^ (if t.dropped = 1 then "" else "s")
    ^ " dropped"
  else ""

(* Writes queued lines to the page in one go (one text node per line, so trimming removes nodes). *)
let flush t =
  t.flush_timer <- None;
  (match t.view with
  | None -> ()
  | Some (el, set_count) ->
      let visible = Browser.client_height el > 0. in
      let at_bottom =
        (not visible)
        || Browser.scroll_top el +. Browser.client_height el
           >= Browser.scroll_height el -. 8.
      in
      let skip = ref 0 in
      let rec drop () =
        if t.pending_drop > 0 then
          match Js.Nullable.toOption (Browser.first_child el) with
          | Some c ->
              Browser.remove_child el c;
              t.pending_drop <- t.pending_drop - 1;
              drop ()
          | None -> ()
      in
      drop ();
      let pending = List.rev t.pending in
      if t.pending_drop > 0 then begin
        skip := min t.pending_drop (List.length pending);
        t.pending_drop <- 0
      end;
      List.iteri
        (fun i l ->
          if i >= !skip then
            Browser.append_child el
              (Browser.create_text_node Browser.document (l ^ "\n")))
        pending;
      if visible && at_bottom then
        Browser.set_scroll_top el (Browser.scroll_height el);
      set_count (count_text t));
  t.pending <- []

let schedule t =
  if t.flush_timer = None then
    t.flush_timer <- Some (Js.Global.setTimeout ~f:(fun () -> flush t) 50)

let step t msg =
  t.last <- msg;
  let line = stamp () ^ "  " ^ msg in
  push_line t line;
  t.bytes <- t.bytes + line_bytes line;
  if t.view <> None then t.pending <- line :: t.pending;
  let drop = ref 0 in
  while t.bytes > max_bytes && t.count - !drop > 1 do
    t.bytes <- t.bytes - line_bytes (nth t !drop);
    incr drop
  done;
  if !drop > 0 then begin
    t.first <- (t.first + !drop) mod Array.length t.lines;
    t.count <- t.count - !drop;
    t.dropped <- t.dropped + !drop;
    if t.view <> None then t.pending_drop <- t.pending_drop + !drop
  end;
  schedule t

(* Logs [msg] only when it differs from the last one logged through the same [memo]. *)
let once t memo msg =
  if !memo <> msg then begin
    memo := msg;
    step t msg
  end

(* The <pre> mounted: show every line kept so far, then follow new ones. *)
let attach t el set_count =
  Browser.set_text_content el "";
  List.iter
    (fun l ->
      Browser.append_child el
        (Browser.create_text_node Browser.document (l ^ "\n")))
    (all_lines t);
  t.pending <- [];
  t.pending_drop <- 0;
  t.view <- Some (el, set_count);
  set_count (count_text t);
  Browser.set_scroll_top el (Browser.scroll_height el)

let detach t = t.view <- None
let text t = String.concat "\n" (all_lines t)
