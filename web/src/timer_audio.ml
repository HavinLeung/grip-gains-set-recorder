(* Beeps (the MVP's audio section, lines 878-949 and 1741-1758), carried over with its comments.

   iOS (Safari and Bluefy) only lets audio start from a tap and suspends it after the screen locks
   or the app is backgrounded, so audio is unlocked and resumed on every tap. Two sound modes,
   chosen after testing on an iPhone in Bluefy and Safari:
    "mix" (default): "ambient" audio. Plays over music from other apps (Spotify) without pausing
           it. Silent mode mutes it on the phone's speaker, but not on headphones (AirPods).
    "over": "playback" audio plus a moment of silence through an <audio> element. Plays in silent
           mode on the speaker, but iOS pauses other apps' music while the page has audio.
   (In a Safari tab, every approach tried paused other music; a home-screen web app mixes, but
   can't use Bluetooth.)

   Which tones play for which event, and their envelope numbers, come from Timer_core.Sound. *)

open Timer_core

type t = {
  mutable actx : Browser.audio_context option;
  mutable silent : Browser.html_audio option;
  mutable clock_check : bool;  (** a clock check is running *)
  logged : string ref;
  log : string -> unit;
}

let create ~log =
  { actx = None; silent = None; clock_check = false; logged = ref ""; log }

(* Logs [msg] unless it repeats the previous audio message. *)
let log_audio t msg =
  if !(t.logged) <> msg then begin
    t.logged := msg;
    t.log msg
  end

(* 800 samples of 8-bit mono silence at 8 kHz (0.1 s), as a WAV blob URL. *)
let silent_wav_url () =
  let n = 800 in
  let b = Array.make (44 + n) 128 in
  let s o str = String.iteri (fun i c -> b.(o + i) <- Char.code c) str in
  let u32 o v =
    b.(o) <- v land 255;
    b.(o + 1) <- (v lsr 8) land 255;
    b.(o + 2) <- (v lsr 16) land 255;
    b.(o + 3) <- (v lsr 24) land 255
  in
  let u16 o v =
    b.(o) <- v land 255;
    b.(o + 1) <- (v lsr 8) land 255
  in
  s 0 "RIFF";
  u32 4 (36 + n);
  s 8 "WAVEfmt ";
  u32 16 16;
  u16 20 1;
  u16 22 1;
  u32 24 8000;
  u32 28 8000;
  u16 32 1;
  u16 34 8;
  s 36 "data";
  u32 40 n;
  Browser.create_object_url
    (Browser.new_blob [| Browser.uint8_of_ints b |] { type_ = "audio/wav" })

(* iOS can leave an AudioContext saying "running" while its clock no longer advances, so nothing
   plays. Like gripgains.ca, check that the clock moves and, if not, replace the context. *)
let check_clock t =
  match t.actx with
  | None -> ()
  | Some _ when t.clock_check -> ()
  | Some _ ->
      t.clock_check <- true;
      ignore
        (Js.Global.setTimeout
           ~f:(fun () ->
             match t.actx with
             | Some ctx when Browser.ctx_state ctx = "running" ->
                 let t0 = Browser.ctx_current_time ctx in
                 ignore
                   (Js.Global.setTimeout
                      ~f:(fun () ->
                        t.clock_check <- false;
                        match t.actx with
                        | Some ctx
                          when Browser.ctx_state ctx = "running"
                               && Browser.ctx_current_time ctx = t0 -> (
                            log_audio t "Audio clock was stuck; restarted audio";
                            (try ignore (Browser.ctx_close ctx) with _ -> ());
                            try
                              let c = Browser.make_audio_context () in
                              t.actx <- Some c;
                              ignore (Browser.attempt (Browser.ctx_resume c))
                            with _ -> t.actx <- None)
                        | _ -> ())
                      160)
             | _ -> t.clock_check <- false)
           200)

let unlock t (s : Settings.t) =
  let over = s.sound_mode = Settings.Over in
  let want = if over then "playback" else "ambient" in
  (match Browser.audio_session () with
  | Some a -> (
      try if Browser.session_type a <> want then Browser.set_session_type a want
      with _ -> ())
  | None -> ());
  (try
     if over then begin
       let el =
         match t.silent with
         | Some el -> el
         | None ->
             let el = Browser.new_audio (silent_wav_url ()) in
             Browser.audio_set_attribute el "playsinline" "";
             t.silent <- Some el;
             el
       in
       match Js.Undefined.toOption (Browser.audio_play el) with
       | Some p -> ignore (Browser.attempt p)
       | None -> ()
     end
     else
       match t.silent with
       | Some el when not (Browser.audio_paused el) -> Browser.audio_pause el
       | _ -> ()
   with _ -> ());
  (try
     let ctx =
       match t.actx with
       | Some c -> c
       | None ->
           let c = Browser.make_audio_context () in
           t.actx <- Some c;
           c
     in
     if Browser.ctx_state ctx <> "running" then
       ignore (Browser.attempt (Browser.ctx_resume ctx));
     (* A silent one-sample buffer, played inside the tap, finishes unlocking older iOS versions. *)
     let b = Browser.create_buffer_source ctx in
     Browser.set_source_buffer b (Browser.create_buffer ctx 1 1 22050);
     Browser.connect_node b (Browser.ctx_destination ctx);
     Browser.start_node b 0.
   with e ->
     log_audio t ("Audio couldn't start: " ^ (Browser.err_of_exn e).name));
  check_clock t;
  ignore
    (Js.Global.setTimeout
       ~f:(fun () ->
         log_audio t
           ("Audio: "
           ^ (match t.actx with
             | Some c -> Browser.ctx_state c
             | None -> "unavailable")
           ^ {js| · audio session |js}
           ^
           match Browser.audio_session () with
           | Some a ->
               "type \"" ^ Browser.session_type a ^ "\" (asked for \"" ^ want
               ^ "\")"
           | None -> "API not available in this browser"))
       300)

(* Any tap or key press re-unlocks audio, e.g. after the screen was locked (see Timer_engine). *)
let needs_unlock t =
  match t.actx with None -> true | Some c -> Browser.ctx_state c <> "running"

let tone t (s : Settings.t) ~freq ~dur =
  match t.actx with
  | None -> ()
  | Some ctx -> (
      try
        if Browser.ctx_state ctx <> "running" then begin
          ignore (Browser.attempt (Browser.ctx_resume ctx));
          log_audio t
            ("Audio: " ^ Browser.ctx_state ctx
           ^ " when a tone was due (tap the screen to restore sound)")
        end;
        let o = Browser.create_oscillator ctx and g = Browser.create_gain ctx in
        let t0 = Browser.ctx_current_time ctx +. 0.01 in
        Browser.set_osc_type o "sine";
        Browser.set_value_at_time (Browser.frequency o) freq t0;
        (* 100% is the original level; 150% stays below clipping *)
        let peak = Sound.peak_gain s in
        let gp = Browser.gain g in
        Browser.set_value_at_time gp 0. t0;
        Browser.linear_ramp gp peak (t0 +. 0.01);
        Browser.set_value_at_time gp peak (t0 +. dur -. 0.03);
        Browser.linear_ramp gp 0. (t0 +. dur);
        (* separate calls: chained connect() fails on older WebKit *)
        Browser.connect_node o g;
        Browser.connect_node g (Browser.ctx_destination ctx);
        Browser.start_node o t0;
        Browser.stop_node o (t0 +. dur +. 0.02)
      with e ->
        let e = Browser.err_of_exn e in
        log_audio t ("Tone failed: " ^ e.name ^ ": " ^ e.message))

(* Plays Sound.tones (each after its delay). *)
let play t s (tones : Sound.tone list) =
  List.iter
    (fun (x : Sound.tone) ->
      if x.delay_ms <= 0. then tone t s ~freq:x.freq ~dur:x.duration
      else
        ignore
          (Js.Global.setTimeout
             ~f:(fun () -> tone t s ~freq:x.freq ~dur:x.duration)
             (int_of_float x.delay_ms)))
    tones
