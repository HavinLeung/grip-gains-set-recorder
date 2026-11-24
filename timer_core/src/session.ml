open! Fp

type phase = Off | Countdown | Rep | Rest
type handler = Idle | Repping | Weighing

type t = {
  settings : Settings.t;
  clock : Clock.t;
  awaiting_battery : bool;
  phase : phase;
  phase_start : float;
  phase_end : float;
  phase_total : float;
  last_beep : int option;
  current : History.set option;
  handler : handler;
  go : float;
  go_device : float option;
  armed : bool;
  holding : bool;
  samples : float list;
  sample_count : int;
  running : Stats.Running.t;
  force : float;
  measured : float option;
  off_target : bool;
  off_dir : float option;
  last_alert : float;
  alert_due : float option;
  live_mean : float option;
  live_sd : float option;
}

type input =
  | Packet of { now : float; view : Protocol.view }
  | Samples of { now : float; samples : Protocol.sample array }
  | Tick of { now : float }
  | Start_set of { now : float; wall_ms : float; session_uuid : string }
  | End_set of { now : float }
  | End_rep of { now : float }
  | Connected of { now : float; fresh : bool }
  | Disconnected of { now : float }
  | Settings_changed of Settings.t

type event =
  | Battery of int
  | Sample of { t : float; force : float }
  | Buffer_cleared
  | Grip_started of float
  | Grip_ended of float
  | Grip_discarded
  | Countdown_beep of int
  | Rep_started
  | Rep_finished of Rep.t
  | Rest_started
  | Set_complete
  | Off_target_alert of { heavy : bool; at : float }
  | Save_set of History.set
  | Set_ended of History.set option

let alert_interval = 500.
let weigh_threshold = 3.
let weigh_window = 160

let init settings =
  {
    settings;
    clock = Clock.empty;
    awaiting_battery = false;
    phase = Off;
    phase_start = 0.;
    phase_end = 0.;
    phase_total = 0.;
    last_beep = None;
    current = None;
    handler = Idle;
    go = 0.;
    go_device = None;
    armed = false;
    holding = false;
    samples = [];
    sample_count = 0;
    running = Stats.Running.empty;
    force = 0.;
    measured = None;
    off_target = false;
    off_dir = None;
    last_alert = Float.neg_infinity;
    alert_due = None;
    live_mean = None;
    live_sd = None;
  }

let in_set st = st.phase <> Off
let samples_array st = Array.of_list (List.rev st.samples)

(* setPhase(phase, secs) *)
let set_phase st phase secs now =
  { st with phase; phase_start = now; phase_total = secs; phase_end = now +. (secs *. 1000.); last_beep = None }

let stop_alerts st = { st with alert_due = None }

(* alertTone(): lastAlert is set whether or not a tone plays. *)
let alert_tone emit st at =
  (match st.off_dir with Some d -> emit (Off_target_alert { heavy = d > 0.; at }) | None -> ());
  { st with last_alert = at }

(* The MVP's setInterval(alertTone, 500), started when force leaves the band.
   It fires at most once per input; if inputs were more than 500 ms apart it
   resumes on its cadence rather than replaying the missed beeps. *)
let fire_alerts emit st now =
  match st.alert_due with
  | Some due when due <= now ->
      let st = if st.off_target then alert_tone emit st due else st in
      let next = ref (due +. alert_interval) in
      while !next <= now do
        next := !next +. alert_interval
      done;
      { st with alert_due = Some !next }
  | _ -> st

let save_current emit st =
  match st.current with Some c when c.reps <> [] -> emit (Save_set c) | _ -> ()

let go_rep emit st now =
  let st = stop_alerts st in
  let go = now /. 1000. in
  let st =
    {
      st with
      handler = Repping;
      go;
      go_device = Clock.device_at st.clock ~page_s:go;
      armed = false;
      samples = [];
      sample_count = 0;
      running = Stats.Running.empty;
      live_mean = None;
      live_sd = None;
      off_target = false;
      off_dir = None;
    }
  in
  emit (Grip_started go);
  let st = set_phase st Rep 0. now in
  emit Rep_started;
  st

(* Fix 3: the duration is measured on the device clock, from GO (as placed on
   it by the first sample after GO) to the ending sample. *)
let finish_rep emit st ~end_page ~end_device =
  let st = stop_alerts st in
  emit (Grip_ended end_page);
  let st = { st with handler = Idle; off_target = false; off_dir = None } in
  let st =
    match st.current with
    | Some c ->
        let go_device = Option.value st.go_device ~default:end_device in
        let rep = Rep.make ~duration:(Float.max 0. (end_device -. go_device)) (samples_array st) ~target_kg:c.target_kg in
        let st = { st with current = Some { c with reps = c.reps @ [ rep ] } } in
        emit (Rep_finished rep);
        save_current emit st;
        st
    | None -> st
  in
  { st with samples = []; sample_count = 0 }

let end_set emit st now =
  let st =
    if st.handler = Repping then
      if st.armed then
        finish_rep emit st
          ~end_page:(match st.clock.last_page with Some l -> l | None -> now /. 1000.)
          ~end_device:st.clock.device_s
      else begin
        let st = stop_alerts st in
        emit Grip_discarded;
        { st with handler = Idle; samples = []; sample_count = 0 }
      end
    else st
  in
  let st = { (stop_alerts st) with handler = Idle } in
  (* Fix 6: no second save here; every rep was saved as it finished. *)
  let finished = st.current in
  let st = set_phase { st with current = None } Off 0. now in
  emit (Set_ended finished);
  st

(* After a rep: the rest, or the end of the set if that was the last rep. *)
let after_rep emit st now =
  match st.current with
  | Some c when List.length c.reps >= c.planned_reps ->
      emit Set_complete;
      end_set emit st now
  | _ ->
      emit Rest_started;
      set_phase st Rest st.settings.rest_s now

(* "End rep": the rep ends at the press as if force had just dropped, engaged
   or not. GO and the press are both placed on the device clock the same way
   ({!Clock.device_at}); if either can't be (no notification on the current
   anchor), the duration is measured on the page clock instead. *)
let end_rep emit st now =
  let end_page = now /. 1000. in
  let st, end_device =
    match (st.go_device, Clock.device_at st.clock ~page_s:end_page) with
    | Some _, Some d -> (st, d)
    | _ -> ({ st with go_device = Some st.go }, end_page)
  in
  after_rep emit (finish_rep emit st ~end_page ~end_device) now

let check_off_target emit st raw now =
  let diff = raw -. st.settings.target in
  if Float.abs diff >= Settings.tolerance_kg st.settings then begin
    let st = { st with off_dir = Some diff } in
    if not st.off_target then begin
      let st = { st with off_target = true } in
      let st = if now -. st.last_alert >= alert_interval then alert_tone emit st now else st in
      { st with alert_due = Some (now +. alert_interval) }
    end
    else st
  end
  else if st.off_target then stop_alerts { st with off_target = false; off_dir = None }
  else st

let push_sample st raw = { st with samples = raw :: st.samples; sample_count = st.sample_count + 1 }

let transition emit st raw (time : Clock.time) now =
  let disp = time.page_s in
  match st.handler with
  | Idle ->
      if (not (in_set st)) && raw >= weigh_threshold then
        { st with samples = [ raw ]; sample_count = 1; handler = Weighing; holding = true; measured = Some raw }
      else st
  | Repping ->
      (* Sampled before GO, arrived late in a batch? Compared on the device
         clock (fix 3). Only if GO came before any notification on the
         current anchor, the first sample decides on the page clock and
         places GO on the device clock by its own mapping. *)
      let before_go = match st.go_device with None -> disp < st.go | Some g -> time.device_s < g in
      let st =
        match st.go_device with
        | None -> { st with go_device = Some (time.device_s -. (disp -. st.go)) }
        | Some _ -> st
      in
      if before_go then st
      else begin
        let st = push_sample st raw in
        let running = Stats.Running.push st.running raw in
        let st =
          { st with running; live_mean = Some (Stats.Running.mean running); live_sd = Some (Stats.Running.sd running) }
        in
        let st = if (not st.armed) && raw >= Settings.engage_kg st.settings then { st with armed = true } else st in
        if st.armed && raw < Settings.fail_kg st.settings then
          after_rep emit (finish_rep emit st ~end_page:disp ~end_device:time.device_s) now
        else if st.armed then check_off_target emit st raw now
        else st
      end
  | Weighing ->
      if in_set st then { st with handler = Idle }
      else if raw >= weigh_threshold then
        if st.holding then
          (* Fix 8: the last [weigh_window] samples, not all since the load went on. *)
          let st = push_sample st raw in
          let st =
            if st.sample_count > weigh_window then { st with samples = List.filteri (fun i _ -> i < weigh_window) st.samples }
            else st
          in
          if st.sample_count mod 8 = 0 then { st with measured = Some (Stats.median (samples_array st)) } else st
        else { st with samples = [ raw ]; sample_count = 1; holding = true; measured = Some raw }
      else if st.holding then
        { st with measured = Some (Stats.trimmed_median (samples_array st)); holding = false; samples = []; sample_count = 0 }
      else { st with handler = Idle }

let on_samples emit st now (samples : Protocol.sample array) =
  (* The mapping of later samples never depends on the transitions, so the
     whole notification is mapped first (the clock as it is after it). *)
  let clock, times = Clock.map_batch st.clock ~now_s:(now /. 1000.) samples in
  let st = ref { st with clock } in
  Array.iteri
    (fun i (s : Protocol.sample) ->
      let time = times.(i) in
      emit (Sample { t = time.page_s; force = s.kg });
      st := transition emit { !st with force = s.kg } s.kg time now)
    samples;
  !st

let tick_set emit st now =
  match st.phase with
  | Countdown | Rest ->
      let left = (st.phase_end -. now) /. 1000. in
      if left <= 0. then go_rep emit st now
      else
        let s = int_of_float (Float.ceil left) in
        if s <= 5 && st.last_beep <> Some s then begin
          emit (Countdown_beep s);
          { st with last_beep = Some s }
        end
        else st
  | Off | Rep -> st

let start_set st ~now ~wall_ms ~session_uuid =
  let s = st.settings in
  let st = { (stop_alerts st) with handler = Idle; samples = []; sample_count = 0; measured = None } in
  let set : History.set =
    {
      id = Jsnum.to_base36 wall_ms;
      source_session_id = Some session_uuid;
      date_time = Jsnum.iso_utc wall_ms;
      gripper = s.gripper;
      side = s.side;
      target_kg = s.target;
      planned_reps = s.planned_reps;
      countdown_s = s.countdown_s;
      rest_s = s.rest_s;
      rep_end_after_pct = s.eng_pct;
      rep_end_below_pct = s.dis_pct;
      tolerance_pct = s.tol_pct;
      reps = [];
      pc = None;
      extra = [];
    }
  in
  set_phase { st with current = Some set } Countdown s.countdown_s now

let input_now = function
  | Packet { now; _ }
  | Samples { now; _ }
  | Tick { now }
  | Start_set { now; _ }
  | End_set { now }
  | End_rep { now }
  | Connected { now; _ }
  | Disconnected { now } ->
      Some now
  | Settings_changed _ -> None

let step st input =
  let evs = ref [] in
  let emit e = evs := e :: !evs in
  let st = match input_now input with Some now -> fire_alerts emit st now | None -> st in
  let st =
    match input with
    | Packet { now; view } -> (
        match Protocol.parse ~awaiting_battery:st.awaiting_battery view with
        | Protocol.Battery_mv mv ->
            emit (Battery (Protocol.battery_percent mv));
            { st with awaiting_battery = false }
        | Protocol.Samples samples -> on_samples emit st now samples
        | Protocol.Ignored -> st)
    | Samples { now; samples } -> on_samples emit st now samples
    | Tick { now } -> tick_set emit st now
    | Start_set { now; wall_ms; session_uuid } ->
        (* The MVP's set button toggles: while a set runs it ends it instead. *)
        if in_set st then st else start_set st ~now ~wall_ms ~session_uuid
    | End_set { now } | Disconnected { now } -> if in_set st then end_set emit st now else st
    | End_rep { now } -> if st.phase = Rep && st.handler = Repping then end_rep emit st now else st
    | Connected { fresh; _ } ->
        let st =
          if fresh then begin
            emit Buffer_cleared;
            {
              st with
              handler = (if in_set st then st.handler else Idle);
              clock = Clock.forget_last (Clock.reset_anchor st.clock);
            }
          end
          else { st with clock = Clock.reset_anchor st.clock }
        in
        { st with awaiting_battery = true }
    | Settings_changed settings -> { st with settings }
  in
  (st, List.rev !evs)

let seconds_left st ~now = Float.max 0. ((st.phase_end -. now) /. 1000.)
let progress st ~now = if st.phase_total <> 0. then seconds_left st ~now /. st.phase_total else 0.
let rep_elapsed st ~now = Float.max 0. ((now /. 1000.) -. st.go)
let reps_done st = match st.current with Some c -> List.length c.reps | None -> 0
let planned_reps st = match st.current with Some c -> c.planned_reps | None -> st.settings.planned_reps
