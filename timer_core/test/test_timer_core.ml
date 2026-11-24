(* Unit tests: XCTest cases ported from grip_gains_companion (marked
   "Swift:"), adapted where the MVP's JS deliberately differs, plus
   hand-written cases for behaviour the Swift app does not have. *)
open Timer_core

let failures = ref 0
let checks = ref 0

let check name cond =
  incr checks;
  if not cond then begin
    incr failures;
    Printf.printf "FAIL %s\n%!" name
  end

let close ?(eps = 1e-9) a b = Float.abs (a -. b) <= eps

let section name f =
  Printf.printf "%s\n%!" name;
  f ()

(* ------------------------------------------------------------------ helpers *)

let settings ?(target = 20.) ?(eng = 50.) ?(dis = 20.) ?(tol = 5.) ?(reps = 4) ?(countdown = 3.) ?(rest = 3.) () =
  {
    Settings.defaults with
    target;
    eng_pct = eng;
    dis_pct = dis;
    tol_pct = tol;
    planned_reps = reps;
    countdown_s = countdown;
    rest_s = rest;
  }

(* A simulated Progressor: one sample per input, 80 Hz, device clock in step
   with the page clock. *)
type sim = { mutable st : Session.t; mutable now : float; mutable ts : float; mutable events : Session.event list }

let sim s = { st = Session.init s; now = 1000.; ts = 0.; events = [] }

let feed sm input =
  let st, evs = Session.step sm.st input in
  sm.st <- st;
  sm.events <- sm.events @ evs;
  evs

let sample sm kg =
  sm.now <- sm.now +. 12.5;
  sm.ts <- sm.ts +. 12500.;
  feed sm (Session.Samples { now = sm.now; samples = [| { Protocol.kg; device_us = sm.ts } |] })

let tick sm = feed sm (Session.Tick { now = sm.now })

(* Time passes without samples reaching the page (the device's counter keeps
   running, as a real Progressor's does). *)
let wait sm ms =
  let until = sm.now +. ms in
  while sm.now < until do
    let step = Float.min until (sm.now +. 100.) -. sm.now in
    sm.now <- sm.now +. step;
    sm.ts <- sm.ts +. (step *. 1000.);
    ignore (tick sm)
  done

(* Start a set and run the countdown until GO, holding [idle_kg] meanwhile. *)
let to_go ?(idle_kg = 0.) sm =
  ignore (feed sm (Session.Start_set { now = sm.now; wall_ms = 1.7e12; session_uuid = "u" }));
  let rec go () =
    if sm.st.handler <> Session.Repping then begin
      ignore (sample sm idle_kg);
      if Float.rem sm.now 100. = 0. then ignore (tick sm);
      go ()
    end
  in
  go ()

let count p l = List.length (List.filter p l)
let is_alert = function Session.Off_target_alert _ -> true | _ -> false
let reps sm = Session.reps_done sm.st

(* -------------------------------------------------------------- statistics *)

let test_stats () =
  (* Swift: StatisticsUtilitiesTests / ProgressorHandlerTests *)
  check "mean empty" (Stats.mean [||] = 0.);
  check "mean single" (Stats.mean [| 5. |] = 5.);
  check "mean" (Stats.mean [| 1.; 2.; 3.; 4.; 5. |] = 3.);
  check "mean negatives" (Stats.mean [| -2.; 0.; 2. |] = 0.);
  check "mean decimals" (close ~eps:1e-4 (Stats.mean [| 1.5; 2.5; 3.0 |]) 2.333333);
  check "median empty" (Stats.median [||] = 0.);
  check "median single" (Stats.median [| 7. |] = 7.);
  check "median odd" (Stats.median [| 3.; 1.; 5.; 2.; 4. |] = 3.);
  check "median even" (Stats.median [| 4.; 1.; 2.; 3. |] = 2.5);
  check "median negatives" (Stats.median [| -5.; -3.; -1.; 0.; 2. |] = -1.);
  check "median decimals" (Stats.median [| 1.5; 2.5; 3.5 |] = 2.5);
  check "sd empty" (Stats.sd [||] = 0.);
  check "sd single" (Stats.sd [| 5. |] = 0.);
  check "sd" (close ~eps:1e-3 (Stats.sd [| 2.; 4.; 4.; 4.; 5.; 5.; 7.; 9. |]) 2.138);
  check "sd identical" (Stats.sd [| 5.; 5.; 5. |] = 0.);
  check "sd two" (close ~eps:1e-3 (Stats.sd [| 1.; 3. |]) 1.414);
  check "sd spread" (close ~eps:0.1 (Stats.sd [| 0.; 100. |]) 70.71);
  check "pop sd empty" (Stats.pop_sd [||] = 0.);
  check "pop sd single" (Stats.pop_sd [| 5. |] = 0.);
  check "pop sd" (close ~eps:1e-3 (Stats.pop_sd [| 2.; 4.; 4.; 4.; 5.; 5.; 7.; 9. |]) 2.0);
  check "pop < sample" (Stats.pop_sd [| 1.; 2.; 3.; 4.; 5. |] < Stats.sd [| 1.; 2.; 3.; 4.; 5. |]);
  let p = Stats.percentile_sorted in
  check "pct empty" (p [||] 0.5 = 0.);
  check "pct 0" (p [| 1.; 2.; 3.; 4.; 5. |] 0. = 1.);
  check "pct 100" (p [| 1.; 2.; 3.; 4.; 5. |] 1. = 5.);
  check "pct 50" (p [| 1.; 2.; 3.; 4.; 5. |] 0.5 = 3.);
  check "pct 25" (p [| 1.; 2.; 3.; 4.; 5. |] 0.25 = 2.);
  check "pct interp" (close ~eps:1e-3 (p [| 1.; 2.; 3.; 4.; 5. |] 0.3) 2.2);
  let tm = Stats.trimmed_median in
  check "trimmed few" (tm [| 1.; 2.; 3.; 4. |] = 2.5);
  check "trimmed few 3" (tm [| 5.; 20.; 10. |] = 10.);
  check "trimmed 10" (tm [| 0.; 1.; 2.; 3.; 4.; 5.; 6.; 7.; 8.; 9. |] = 4.5);
  check "trimmed fraction" (tm ~fraction:0.1 [| 0.; 1.; 2.; 3.; 4.; 5.; 6.; 7.; 8.; 9. |] = 4.5);
  check "trimmed pickup/release" (tm [| 5.; 10.; 15.; 20.; 20.; 20.; 20.; 15.; 10.; 5. |] = 20.);
  check "trimmed five" (tm [| 5.; 20.; 20.; 20.; 5. |] = 20.);
  let v = [| 0.; 5.; 10.; 20.; 20.; 20.; 20.; 10.; 5.; 0. |] in
  check "trimmed vs median" (Stats.median v = 10. && tm v = 20.);
  let rnd = Random.State.make [| 7 |] in
  let realistic =
    Array.concat
      [
        Array.init 30 (fun i -> float_of_int i *. 20. /. 30.);
        Array.init 40 (fun _ -> 20. +. Random.State.float rnd 1. -. 0.5);
        Array.init 30 (fun i -> 20. -. (float_of_int i *. 20. /. 30.));
      ]
  in
  check "trimmed realistic" (close ~eps:1. (tm realistic) 20.);
  (* Swift: testRunningStatisticsMatchesBatchCalculationAfterEachValue *)
  let r = ref Stats.Running.empty and acc = ref [] and ok = ref true in
  for _ = 1 to 500 do
    let x = 20. +. Random.State.float rnd 4. -. 2. in
    r := Stats.Running.push !r x;
    acc := x :: !acc;
    let a = Array.of_list (List.rev !acc) in
    if not (close (Stats.Running.mean !r) (Stats.mean a) && close (Stats.Running.sd !r) (Stats.sd a)) then ok := false
  done;
  check "welford matches batch" !ok;
  check "welford empty" (Stats.Running.mean Stats.Running.empty = 0. && Stats.Running.sd Stats.Running.empty = 0.)

(* --------------------------------------------------------------------- reps *)

let test_reps () =
  (* Swift: RepResultTests *)
  let a = Rep.analyze [| 10.; 11.; 12. |] ~target_kg:0. in
  check "under 4 samples: whole rep" (a.stable = (0, 2));
  check "empty" ((Rep.analyze [||] ~target_kg:0.).stable = (0, 0) && (Rep.analyze [||] ~target_kg:0.).median = 0.);
  let samples = [| 5.; 10.; 15.; 20.; 20.1; 19.9; 20.; 20.2; 19.8; 20.; 20.1; 15.; 10.; 5. |] in
  let a = Rep.analyze samples ~target_kg:20. in
  check "stable median" (close ~eps:0.2 a.median 20.);
  check "stable start skips pickup" (fst a.stable >= 3);
  check "stable end skips release" (snd a.stable <= 10);
  let s2 = [| 5.; 10.; 20.; 20.; 20.; 20.; 10.; 5. |] in
  let a2 = Rep.analyze s2 ~target_kg:20. in
  let lo, hi = a2.stable in
  check "stats over the stable slice" (a2.median = Stats.median (Array.sub s2 lo (hi - lo + 1)));
  let s3 = Array.concat [ Array.make 5 0.; Array.make 20 10.; Array.make 5 0. ] in
  let a3 = Rep.analyze s3 ~target_kg:10. in
  check "filtered median" (close ~eps:0.1 a3.median 10.);
  check "filtered sd < raw sd" (a3.sd < Stats.sd s3);
  check "mean" (close ~eps:1e-3 (Rep.analyze (Array.make 20 15.) ~target_kg:0.).mean 15.);
  check "sd identical" ((Rep.analyze (Array.make 20 20.) ~target_kg:0.).sd = 0.);
  check "deviation" (close ~eps:1e-3 (Rep.analyze (Array.make 20 20.5) ~target_kg:20.).dev_kg 0.5);
  let set_with target reps : History.set =
    {
      id = "x";
      source_session_id = None;
      date_time = "";
      gripper = "crusher";
      side = "left";
      target_kg = target;
      planned_reps = 4;
      countdown_s = 20.;
      rest_s = 20.;
      rep_end_after_pct = 90.;
      rep_end_below_pct = 70.;
      tolerance_pct = 7.5;
      reps;
      pc = None;
      extra = [];
    }
  in
  let rep v n = Rep.make ~duration:7. (Array.make n v) ~target_kg:20. in
  let pct st r = fst (Summary.rep_deviation st r) in
  check "deviation % +" (close ~eps:1e-3 (pct (set_with 20. []) (rep 22. 20)) 10.);
  check "deviation % -" (close ~eps:1e-3 (pct (set_with 20. []) (rep 18. 20)) (-10.));
  check "deviation % zero target" (pct (set_with 0. []) (rep 20. 20) = 0.);
  let q = (Rep.analyze (Array.init 20 (fun i -> float_of_int (i + 1))) ~target_kg:0.).pct in
  check "quartiles ordered" (q.q1 <= q.med && q.med <= q.q3 && q.p1 <= q.p99);
  (* Saved rep: 20 Hz graph, stable indices scaled. *)
  let r = Rep.make ~duration:2.345 (Array.init 10 float_of_int) ~target_kg:4. in
  check "force thinned" (r.force_kg = [| 1.5; 5.5; 8.5 |]);
  check "seconds r2" (r.seconds = 2.35);
  check "graph hz" (r.graph_hz = 20.);
  check "stable /4" (r.stable = (0, 2));
  (* Swift: SetStatisticsTests *)
  let mk v target = Rep.make ~duration:7. (Array.make 70 v) ~target_kg:target in
  let s = Summary.of_set (set_with 12. [ mk 11.9 12.; mk 12.1 12. ]) in
  check "mean deviation averages reps" (close ~eps:0.01 s.dev_kg 0.);
  let s = Summary.of_set (set_with 12. [ mk 11.9 12.; mk 12. 12. ]) in
  check "mean deviation asymmetric" (close ~eps:0.01 s.dev_kg (-0.05));
  let s = Summary.of_set (set_with 12. [ mk 20. 12.; mk 25. 12. ]) in
  check "average sd" (s.avg_sd_kg = 0. && s.reps = 2 && s.total_seconds = 14.);
  check "summary of nothing" ((Summary.of_set (set_with 12. [])).avg_median_kg = 0.)

(* ----------------------------------------------------------------- protocol *)

let test_protocol () =
  (* Swift: DeviceBatteryTests *)
  let v = Protocol.view_of_array in
  check "battery 3950 mV"
    (Protocol.parse ~awaiting_battery:true (v [| 0x00; 0x04; 0x6E; 0x0F; 0x00; 0x00 |]) = Protocol.Battery_mv 3950.);
  let sliced = [| 0xFF; 0x00; 0x04; 0x6E; 0x0F; 0x00; 0x00 |] in
  let view = { Protocol.length = 6; get_u8 = (fun i -> sliced.(i + 1)) } in
  check "battery from an offset view" (Protocol.parse ~awaiting_battery:true view = Protocol.Battery_mv 3950.);
  let not_battery a = match Protocol.parse ~awaiting_battery:true (v a) with Protocol.Battery_mv _ -> false | _ -> true in
  check "weight packet is not battery" (not_battery [| 0x01; 0x04; 0x6E; 0x0F; 0x00; 0x00 |]);
  check "short is not battery" (not_battery [| 0x00; 0x04; 0x6E; 0x0F; 0x00 |]);
  check "mislabelled is not battery" (not_battery [| 0x00; 0x02; 0x6E; 0x0F; 0x00; 0x00 |]);
  check "empty is not battery" (not_battery [||]);
  check "battery only when awaited"
    (Protocol.parse ~awaiting_battery:false (v [| 0x00; 0x04; 0x6E; 0x0F; 0x00; 0x00 |]) = Protocol.Ignored);
  let bp = Protocol.battery_percent in
  check "curve points" (bp 4200. = 100 && bp 3820. = 50 && bp 3300. = 0);
  check "interpolates" (bp 4150. = 95 && bp 3710. = 15);
  check "clamped" (bp 4350. = 100 && bp 3000. = 0 && bp 0. = 0);
  let mono = ref true and prev = ref 100 in
  let mv = ref 4300. in
  while !mv >= 3200. do
    let p = bp !mv in
    if p > !prev || p < 0 || p > 100 then mono := false;
    prev := p;
    mv := !mv -. 5.
  done;
  check "never increases as voltage drops" !mono;
  (* Hand-written: weight packets. 20.5 kg = 0x41A40000; 1.0 = 0x3F800000. *)
  let pkt =
    [| 0x01; 0x10; 0x00; 0x00; 0xA4; 0x41; 0x10; 0x27; 0x00; 0x00; 0x00; 0x00; 0x80; 0xBF; 0xFF; 0xFF; 0xFF; 0xFF; 0x99 |]
  in
  (match Protocol.parse ~awaiting_battery:true (v pkt) with
  | Protocol.Samples [| a; b |] ->
      check "float32" (a.kg = 20.5 && b.kg = -1.0);
      check "uint32" (a.device_us = 10000. && b.device_us = 4294967295.)
  | _ -> check "two samples, trailing byte ignored" false);
  check "denormal" (Protocol.float32_le (v [| 1; 0; 0; 0 |]) 0 = Float.ldexp 1. (-149));
  check "infinity" (Protocol.float32_le (v [| 0; 0; 0x80; 0x7F |]) 0 = Float.infinity);
  check "nan" (Float.is_nan (Protocol.float32_le (v [| 1; 0; 0x80; 0x7F |]) 0));
  check "type 1 short" (Protocol.parse ~awaiting_battery:false (v [| 1; 0; 0; 0; 0 |]) = Protocol.Ignored);
  check "type 1 no whole pair" (Protocol.parse ~awaiting_battery:false (v [| 1; 0; 0; 0; 0; 0 |]) = Protocol.Samples [||]);
  check "commands"
    (List.map Protocol.command_byte [ Tare; Start_measuring; Shutdown; Battery ] = [ 0x64; 0x65; 0x6E; 0x6F ])

(* -------------------------------------------------------- force handler *)

let test_thresholds () =
  (* Swift: testEngageAtExactPercentageThreshold / testNoEngageBelowPercentageThreshold *)
  let sm = sim (settings ()) in
  to_go sm;
  ignore (sample sm 9.99);
  check "no engage below threshold" (not sm.st.armed);
  ignore (sample sm 10.);
  check "engage at exactly the threshold" sm.st.armed;
  (* Swift: testDisengageAtExactPercentageThreshold (uses <) *)
  ignore (sample sm 4.);
  check "still engaged at exactly the disengage threshold" (sm.st.handler = Session.Repping);
  (* Swift: testDisengageBelowPercentageThreshold *)
  ignore (sample sm 3.99);
  check "disengages below the threshold" (sm.st.handler = Session.Idle && reps sm = 1 && sm.st.phase = Session.Rest);
  (* Swift: testDisengagePercentageMustBeLessThanEngage *)
  let sm = sim (settings ~eng:50. ~dis:45. ()) in
  to_go sm;
  ignore (sample sm 10.);
  ignore (sample sm 9.);
  check "engaged at the disengage threshold" (sm.st.handler = Session.Repping);
  (* Swift: testThresholdsUpdateWhenTargetChanges (here: mid-rep, from the live settings) *)
  let sm = sim (settings ()) in
  to_go sm;
  ignore (sample sm 5.);
  check "not engaged at 5 kg with 20 kg target" (not sm.st.armed);
  ignore (feed sm (Session.Settings_changed (settings ~target:10. ())));
  ignore (sample sm 5.);
  check "engaged at 5 kg with 10 kg target" sm.st.armed

let test_off_target () =
  (* Swift: testOnTargetWithinPercentageTolerance / testOffTargetOutsidePercentageTolerance *)
  let sm = sim (settings ()) in
  to_go sm;
  ignore (sample sm 12.);
  ignore (sample sm 20.5);
  check "on target within tolerance" (not sm.st.off_target);
  let evs = sample sm 21.5 in
  check "off target outside tolerance" sm.st.off_target;
  (* 12 kg was already off target (light) and alerted; re-entering the band
     edge within 500 ms does not alert again at once. *)
  check "no immediate re-alert" (count is_alert evs = 0 && count is_alert sm.events = 1);
  (* Swift: testAtToleranceBoundary / testJustUnderToleranceBoundary (>=) *)
  let sm = sim (settings ~target:10. ~tol:5. ()) in
  to_go sm;
  ignore (sample sm 10.);
  ignore (sample sm 10.49);
  check "just under boundary is on target" (not sm.st.off_target);
  ignore (sample sm 10.5);
  check "boundary is off target" sm.st.off_target;
  (* Swift: testOffTargetDirectionWithPercentageTolerance *)
  let sm = sim (settings ()) in
  to_go sm;
  ignore (sample sm 12.);
  ignore (sample sm 22.);
  check "heavy direction" (sm.st.off_dir = Some 2.);
  ignore (sample sm 18.);
  check "light direction" (sm.st.off_dir = Some (-2.));
  (* Swift: testOffTargetAlertsDoNotRepeatWhileForceFlickersAcrossToleranceEdge *)
  let sm = sim (settings ()) in
  to_go sm;
  ignore (sample sm 20.);
  sm.events <- [];
  for i = 0 to 39 do
    ignore (sample sm (if i mod 2 = 0 then 21.05 else 20.95))
  done;
  check "flicker alerts once in 0.5 s" (count is_alert sm.events = 1);
  (* Hand-written: the repeating alert, every 500 ms while off target. *)
  let sm = sim (settings ()) in
  to_go sm;
  ignore (sample sm 20.);
  sm.events <- [];
  ignore (sample sm 25.);
  wait sm 1600.;
  let alerts = List.filter_map (function Session.Off_target_alert { at; heavy } -> Some (at, heavy) | _ -> None) sm.events in
  check "alert every 500 ms" (List.length alerts = 4 && List.for_all snd alerts);
  (match alerts with
  | (t0, _) :: (t1, _) :: _ -> check "alert cadence" (t1 -. t0 = 500.)
  | _ -> check "alert cadence" false);
  ignore (sample sm 20.);
  sm.events <- [];
  wait sm 1000.;
  check "alerts stop on target" (count is_alert sm.events = 0)

let test_rep_flow () =
  (* Intended: a rep that never reaches the engage threshold never ends. *)
  let sm = sim (settings ()) in
  to_go sm;
  for _ = 1 to 80 * 60 do
    ignore (sample sm 9.)
  done;
  wait sm 100.;
  check "never-engaging rep keeps running" (sm.st.handler = Session.Repping && sm.st.phase = Session.Rep && reps sm = 0);
  let evs = feed sm (Session.End_set { now = sm.now }) in
  check "ending it discards the grip, no rep"
    (List.mem Session.Grip_discarded evs
    && List.exists (function Session.Set_ended (Some s) -> s.reps = [] | _ -> false) evs
    && sm.st.phase = Session.Off);
  (* Intended: the rest timer starts the next rep whether or not the user grips. *)
  let sm = sim (settings ~reps:3 ()) in
  to_go sm;
  ignore (sample sm 20.);
  ignore (sample sm 0.);
  check "rest after rep 1" (sm.st.phase = Session.Rest);
  wait sm 3100.;
  check "next rep starts without gripping" (sm.st.phase = Session.Rep && sm.st.handler = Session.Repping);
  (* Set completion after the planned reps. *)
  let sm = sim (settings ~reps:2 ()) in
  to_go sm;
  for _ = 1 to 200 do
    ignore (sample sm 20.)
  done;
  ignore (sample sm 0.);
  wait sm 3100.;
  for _ = 1 to 100 do
    ignore (sample sm 20.)
  done;
  sm.events <- [];
  ignore (sample sm 0.);
  check "set complete" (List.mem Session.Set_complete sm.events && sm.st.phase = Session.Off);
  (match List.filter_map (function Session.Set_ended s -> s | _ -> None) sm.events with
  | [ s ] ->
      check "two reps saved" (List.length s.reps = 2);
      check "rep 1 ~2.5 s" (close ~eps:0.05 (List.hd s.reps).seconds 2.5)
  | _ -> check "set ended once" false);
  (* Fix 6: each rep saves the set once as it finishes; the end of the set
     (planned reps done, or "End set early") does not save it again. *)
  let saves sm = count (function Session.Save_set _ -> true | _ -> false) sm.events in
  let sm = sim (settings ~reps:2 ()) in
  to_go sm;
  for _ = 1 to 100 do
    ignore (sample sm 20.)
  done;
  ignore (sample sm 0.);
  check "rep 1 saves once" (saves sm = 1);
  wait sm 3100.;
  for _ = 1 to 100 do
    ignore (sample sm 20.)
  done;
  ignore (sample sm 0.);
  check "completed set: one save per rep, none extra at the end" (saves sm = 2 && sm.st.phase = Session.Off);
  (match List.filter_map (function Session.Save_set s -> Some (List.length s.reps) | _ -> None) sm.events with
  | [ 1; 2 ] -> check "the last save has every rep" true
  | _ -> check "the last save has every rep" false);
  let sm = sim (settings ~reps:3 ()) in
  to_go sm;
  for _ = 1 to 100 do
    ignore (sample sm 20.)
  done;
  ignore (sample sm 0.);
  wait sm 3100.;
  for _ = 1 to 100 do
    ignore (sample sm 20.)
  done;
  ignore (feed sm (Session.End_set { now = sm.now }));
  check "ended early mid-rep: the engaged rep saves once, the end adds nothing" (saves sm = 2);
  let sm = sim (settings ~reps:3 ()) in
  to_go sm;
  for _ = 1 to 100 do
    ignore (sample sm 20.)
  done;
  ignore (sample sm 0.);
  ignore (feed sm (Session.End_set { now = sm.now }));
  check "ended early during the rest: no extra save" (saves sm = 1);
  (* Countdown beeps (Swift: CountdownSoundTests): 5…1 once each, then GO. *)
  let sm = sim (settings ~countdown:7. ()) in
  ignore (feed sm (Session.Start_set { now = sm.now; wall_ms = 0.; session_uuid = "u" }));
  wait sm 7100.;
  let beeps = List.filter_map (function Session.Countdown_beep s -> Some s | _ -> None) sm.events in
  check "countdown beeps 5..1" (beeps = [ 5; 4; 3; 2; 1 ]);
  check "then GO" (List.mem Session.Rep_started sm.events);
  (* A new rest re-arms the beeps (Swift: resets above five seconds). *)
  ignore (sample sm 20.);
  sm.events <- [];
  ignore (sample sm 0.);
  wait sm 3100.;
  let beeps = List.filter_map (function Session.Countdown_beep s -> Some s | _ -> None) sm.events in
  check "rest beeps 3..1" (beeps = [ 3; 2; 1 ]);
  (* Samples stamped before GO are not part of the rep. *)
  let sm = sim (settings ()) in
  ignore (feed sm (Session.Start_set { now = sm.now; wall_ms = 0.; session_uuid = "u" }));
  while sm.st.handler <> Session.Repping do
    sm.now <- sm.now +. 100.;
    ignore (tick sm)
  done;
  (* The first batch is anchored with its newest sample at "now", so its
     older samples land before GO. *)
  let batch = Array.init 16 (fun k -> { Protocol.kg = 30.; device_us = sm.ts +. (float_of_int (k + 1) *. 12500.) }) in
  ignore (feed sm (Session.Samples { now = sm.now +. 20.; samples = batch }));
  check "pre-GO samples ignored" (sm.st.sample_count = 2);
  (* End set early while engaged keeps the rep. *)
  let sm = sim (settings ()) in
  to_go sm;
  for _ = 1 to 80 do
    ignore (sample sm 20.)
  done;
  let evs = feed sm (Session.End_set { now = sm.now }) in
  check "end early keeps an engaged rep"
    (List.exists (function Session.Set_ended (Some s) -> List.length s.reps = 1 | _ -> false) evs);
  check "start/end are toggles"
    ((fst (Session.step sm.st (Session.End_set { now = sm.now }))).phase = Session.Off)

let test_end_rep () =
  let saves sm = count (function Session.Save_set _ -> true | _ -> false) sm.events in
  let finished sm = List.filter_map (function Session.Rep_finished r -> Some r | _ -> None) sm.events in
  let end_rep sm = feed sm (Session.End_rep { now = sm.now }) in
  (* A rep that never engages: End rep keeps it, timed from GO to the press,
     with the stats of the light hold, then the rest as after any rep. *)
  let sm = sim (settings ~reps:2 ()) in
  to_go sm;
  sm.events <- [];
  for _ = 1 to 160 do
    ignore (sample sm 9.)
  done;
  let evs = end_rep sm in
  check "end rep: never engaged, rep kept" (reps sm = 1 && sm.st.phase = Session.Rest && sm.st.handler = Session.Idle);
  check "end rep: same events as a natural end"
    (match evs with
    | [ Session.Grip_ended _; Session.Rep_finished _; Session.Save_set s; Session.Rest_started ] -> List.length s.reps = 1
    | _ -> false);
  (match finished sm with
  | [ r ] ->
      check "end rep: GO to the press, on the device clock" (close ~eps:0.02 r.seconds 2.0);
      check "end rep: stats from the samples so far" (r.median_kg = 9. && r.mean_kg = 9. && Array.length r.force_kg = 40)
  | _ -> check "end rep: one rep" false);
  check "end rep: grip shading closes at the press"
    (List.exists (function Session.Grip_ended t -> t = sm.now /. 1000. | _ -> false) evs);
  (* Ignored in the rest, and the force dropping afterwards ends nothing. *)
  let before = sm.st in
  check "end rep: ignored in the rest" (end_rep sm = [] && sm.st == before);
  for _ = 1 to 10 do
    ignore (sample sm 0.)
  done;
  check "end rep: nothing more after it" (reps sm = 1 && saves sm = 1);
  (* The next (last) rep, engaged and still held: End rep ends it and the set. *)
  wait sm 3100.;
  check "end rep: next rep started" (sm.st.phase = Session.Rep);
  sm.events <- [];
  for _ = 1 to 120 do
    ignore (sample sm 20.)
  done;
  let expected = (sm.now /. 1000.) -. sm.st.go in
  ignore (end_rep sm);
  check "end rep: last rep completes the set" (List.mem Session.Set_complete sm.events && sm.st.phase = Session.Off);
  (match List.filter_map (function Session.Set_ended s -> s | _ -> None) sm.events with
  | [ s ] ->
      check "end rep: both reps in the set" (List.length s.reps = 2);
      check "end rep: engaged rep timed to the press" (close ~eps:0.02 (List.nth s.reps 1).seconds expected && expected > 1.5);
      check "end rep: engaged rep's median" ((List.nth s.reps 1).median_kg = 20.)
  | _ -> check "end rep: set ended once" false);
  check "end rep: one save for the last rep, none extra" (saves sm = 1);
  (* No samples at all since GO: zeros and an empty graph, no crash. *)
  let sm = sim (settings ()) in
  to_go sm;
  sm.events <- [];
  wait sm 1500.;
  ignore (end_rep sm);
  (match finished sm with
  | [ r ] ->
      check "end rep: empty rep timed" (close ~eps:0.02 r.seconds 1.5);
      check "end rep: empty rep zeros"
        (r.median_kg = 0. && r.mean_kg = 0. && r.sd_kg = 0. && r.force_kg = [||] && r.stable = (0, 0))
  | _ -> check "end rep: empty rep kept" false);
  (* Never any notification (nothing to place GO on the device clock): page clock. *)
  let sm = sim (settings ()) in
  ignore (feed sm (Session.Start_set { now = sm.now; wall_ms = 0.; session_uuid = "u" }));
  while sm.st.phase <> Session.Rep do
    sm.now <- sm.now +. 100.;
    ignore (tick sm)
  done;
  sm.now <- sm.now +. 1234.;
  ignore (end_rep sm);
  (match finished sm with
  | [ r ] -> check "end rep: page clock without notifications" (close ~eps:1e-9 r.seconds 1.23)
  | _ -> check "end rep: kept without notifications" false);
  (* Ignored outside a set and in the countdown. *)
  let sm = sim (settings ()) in
  let st0 = sm.st in
  check "end rep: ignored outside a set" (end_rep sm = [] && sm.st == st0);
  ignore (feed sm (Session.Start_set { now = sm.now; wall_ms = 0.; session_uuid = "u" }));
  let st1 = sm.st in
  check "end rep: ignored in the countdown" (end_rep sm = [] && sm.st == st1 && sm.st.phase = Session.Countdown)

let test_weighing () =
  (* Swift: testWeightCalibration* (the MVP weighs only outside a set) *)
  let sm = sim (settings ()) in
  ignore (sample sm 2.5);
  check "no weighing below 3 kg" (sm.st.handler = Session.Idle);
  ignore (sample sm 10.);
  check "weighing at 3 kg or more" (sm.st.handler = Session.Weighing && sm.st.holding && sm.st.measured = Some 10.);
  ignore (sample sm 12.);
  ignore (sample sm 11.);
  (* The Swift app re-medians every sample (11 here); the MVP every 8th. *)
  check "measured updates every 8 samples" (sm.st.measured = Some 10.);
  for _ = 1 to 5 do
    ignore (sample sm 11.)
  done;
  check "median after 8" (sm.st.measured = Some 11.);
  ignore (sample sm 2.);
  check "released: trimmed median kept" (sm.st.measured = Some 11. && not sm.st.holding);
  ignore (sample sm 2.);
  check "then idle" (sm.st.handler = Session.Idle);
  ignore (sample sm 5.);
  ignore (feed sm (Session.Start_set { now = sm.now; wall_ms = 0.; session_uuid = "u" }));
  check "start set clears measured" (sm.st.measured = None);
  ignore (sample sm 5.);
  check "weighing stops in a set" (sm.st.handler = Session.Idle);
  (* Fix 8: a sliding 2 s window. 5 s of 10 kg, then a plate added: 3 s of
     15 kg. The MVP's median of everything since the load went on still said
     10 kg; the window says 15 within a second. *)
  let sm = sim (settings ()) in
  for _ = 1 to 400 do
    ignore (sample sm 10.)
  done;
  check "window: 10 kg" (sm.st.measured = Some 10. && List.length sm.st.samples = Session.weigh_window);
  for _ = 1 to 72 do
    ignore (sample sm 15.)
  done;
  check "window: half a second in, still 10 kg" (sm.st.measured = Some 10.);
  for _ = 1 to 88 do
    ignore (sample sm 15.)
  done;
  check "window: a plate added shows within a second" (sm.st.measured = Some 15.);
  for _ = 1 to 80 do
    ignore (sample sm 15.)
  done;
  (* Lifting off: 6 samples coming down, then under 3 kg. *)
  List.iter (fun kg -> ignore (sample sm kg)) [ 12.; 9.; 7.; 5.; 4.; 3.5; 1. ];
  check "window: release keeps the middle of the last 2 s" (sm.st.measured = Some 15. && not sm.st.holding);
  (* The cadence stays every 8th sample of the hold once the window is full
     (the window's length alone would be a multiple of 8 on every sample). *)
  let sm = sim (settings ()) in
  for _ = 1 to 400 do
    ignore (sample sm 10.)
  done;
  let changes = ref 0 and on_cadence = ref true and prev = ref sm.st.measured in
  for i = 1 to 160 do
    ignore (sample sm (10. +. float_of_int i));
    if sm.st.measured <> !prev then begin
      incr changes;
      if sm.st.sample_count mod 8 <> 0 then on_cadence := false
    end;
    prev := sm.st.measured
  done;
  (* The median moves once half the window is new: at 80, 88, … 160. *)
  check "window: re-median every 8th sample" (!changes = 11 && !on_cadence);
  (* A hold of 2 s or less: the same as the MVP, the whole hold's trimmed
     median (here the 5 kg hang-on and lift-off samples are trimmed). *)
  let sm = sim (settings ()) in
  let hold = Array.concat [ Array.make 30 5.; Array.make 40 20.; Array.make 30 5. ] in
  Array.iter (fun kg -> ignore (sample sm kg)) hold;
  ignore (sample sm 0.);
  check "window: short hold as before" (sm.st.measured = Some (Stats.trimmed_median hold) && sm.st.measured = Some 20.)

let test_clock () =
  let map_pages c ~now_s b =
    let c, t = Clock.map_batch c ~now_s b in
    (c, Array.map (fun (x : Clock.time) -> x.page_s) t)
  in
  (* Swift: testTimestampCounterRestartDoesNotCrash *)
  let sm = sim (settings ()) in
  sm.ts <- 500000.;
  ignore (sample sm 0.);
  sm.ts <- 0.;
  ignore (sample sm 0.);
  ignore (sample sm 0.);
  let times = List.filter_map (function Session.Sample { t; _ } -> Some t | _ -> None) sm.events in
  check "monotonic after counter restart" (List.length times = 3 && times = List.sort compare times);
  check "not ahead of now" (List.for_all (fun t -> t <= (sm.now /. 1000.) +. 0.05) times);
  (* Swift: testGripDurationPreservedAcrossTimestampRestart *)
  let sm = sim (settings ~target:10. ~eng:30. ~dis:10. ()) in
  to_go sm;
  for _ = 1 to 161 do
    ignore (sample sm 10.)
  done;
  sm.ts <- 0.;
  for _ = 1 to 80 do
    ignore (sample sm 10.)
  done;
  ignore (sample sm 0.);
  (match sm.st.current with
  | Some { reps = [ r ]; _ } -> check "duration across restart" (close ~eps:0.05 r.seconds 3.0)
  | _ -> check "one rep" false);
  (* Hand-written: batch anchoring, stall jump, drift easing. *)
  let batch ts0 n = Array.init n (fun i -> { Protocol.kg = 0.; device_us = ts0 +. (float_of_int i *. 12500.) }) in
  let c, t = map_pages Clock.empty ~now_s:10. (batch 1e6 16) in
  check "first batch ends at now" (t.(15) = 10. && close t.(0) (10. -. 0.1875));
  (* Next batch arrives 0.2 s later: on time, nothing to ease. *)
  let c, t = map_pages c ~now_s:10.2 (batch (1e6 +. 200000.) 16) in
  check "on-time batch" (close t.(15) 10.2);
  (* A 1.5 s stall: the batch's samples are old relative to now; jump. *)
  let c, _ = map_pages c ~now_s:11.9 (batch (1e6 +. 400000.) 16) in
  check "stall recovery jumps to now"
    (match c.last_page with Some _ -> close (Option.get c.anchor).page (10. -. 0.1875 +. 1.5) | None -> false);
  (* Device 1% fast: the anchor eases toward now by 5% of the lead. *)
  let c0, _ = map_pages Clock.empty ~now_s:0. (batch 0. 16) in
  let c = ref c0 in
  for k = 1 to 300 do
    c := fst (map_pages !c ~now_s:(0.2 *. float_of_int k) (batch (float_of_int k *. 202000.) 16))
  done;
  let lead = Option.get !c.last_page -. 60. in
  check "drift bounded" (Float.abs lead < 0.05);
  (* Fix 4: a wrap of the uint32 counter is not a restart. *)
  let c, _ = map_pages Clock.empty ~now_s:5. (batch (4294967296. -. 100000.) 16) in
  let a0 = c.anchor in
  let wrapping = Array.init 16 (fun i -> { Protocol.kg = 0.; device_us = Jsnum.to_uint32 (100000. +. (float_of_int i *. 12500.)) }) in
  let c, t = map_pages c ~now_s:5.2 wrapping in
  check "wrap keeps the anchor" (Option.map (fun (a : Clock.anchor) -> a.device) c.anchor = Option.map (fun (a : Clock.anchor) -> a.device) a0);
  check "wrap keeps 12.5 ms steps" (Array.for_all Fun.id (Array.init 15 (fun i -> close ~eps:1e-9 (t.(i + 1) -. t.(i)) 0.0125)));
  check "wrap lands on time" (close ~eps:1e-6 t.(15) 5.2);
  (* Fix 4: a restart in the middle of a notification (tare): the samples
     before it keep their times, the newest lands at now, none is ahead. *)
  let c, t0 = map_pages Clock.empty ~now_s:20. (batch 5e6 16) in
  let tared = Array.init 16 (fun i -> { Protocol.kg = 0.; device_us = (if i < 6 then 5e6 +. (float_of_int (16 + i) *. 12500.) else float_of_int (i - 6) *. 12500.) }) in
  let _, t = map_pages c ~now_s:20.2 tared in
  check "restart mid-notification: earlier samples continue" (close ~eps:1e-9 (t.(0) -. t0.(15)) 0.0125);
  check "restart mid-notification: newest at now" (close ~eps:1e-9 t.(15) 20.2);
  check "restart mid-notification: nothing ahead of now" (Array.for_all (fun x -> x <= 20.2 +. 1e-9) t);
  check "restart mid-notification: monotonic" (Array.for_all Fun.id (Array.init 15 (fun i -> t.(i + 1) > t.(i))));
  (* Fix 4: reconnecting to a device whose counter restarted (switched off and
     on, or measuring restarted) keeps the anchor placed for the first
     notification: newest at now, oldest 187.5 ms before. The MVP replaced it
     with one putting the oldest at now and the newest 187.5 ms ahead. *)
  let c, _ = map_pages Clock.empty ~now_s:30. (batch 80e6 16) in
  let c, t = map_pages (Clock.reset_anchor c) ~now_s:45. (batch 2e5 16) in
  check "reconnect after a restart: newest at now" (close ~eps:1e-9 t.(15) 45. && close ~eps:1e-9 t.(0) (45. -. 0.1875));
  check "reconnect after a restart: one anchor" (Option.map (fun (a : Clock.anchor) -> a.device) c.anchor = Some 2e5);
  (* Fix 4: a restart that reads as a forward jump modulo 2^32 (old counter
     near the top of its range) would land far ahead of now: re-anchored. *)
  let c, _ = map_pages Clock.empty ~now_s:50. (batch (4294967296. -. 30e6) 16) in
  let _, t = map_pages c ~now_s:50.2 (batch 0. 16) in
  check "restart read as a jump forward is caught" (close ~eps:1e-9 t.(15) 50.2);
  (* Fix 4: 75 minutes on one connection, through a wrap: the anchor is moved
     along (same mapping), so the samples stay on time throughout. *)
  let c = ref Clock.empty and worst = ref 0. and steps_ok = ref true and prev = ref Float.neg_infinity in
  for k = 0 to (75 * 60 * 5) - 1 do
    let ts0 = float_of_int k *. 200000. in
    let b = Array.init 16 (fun i -> { Protocol.kg = 0.; device_us = Jsnum.to_uint32 (ts0 +. (float_of_int i *. 12500.)) }) in
    let now_s = 1. +. ((ts0 +. 187500.) /. 1e6) in
    let c', t = map_pages !c ~now_s b in
    c := c';
    worst := Float.max !worst (Float.abs (t.(15) -. now_s));
    Array.iter
      (fun x ->
        if not (x > !prev) then steps_ok := false;
        prev := x)
      t
  done;
  check "75 minutes through a wrap: on time" (!worst < 1e-6);
  check "75 minutes through a wrap: monotonic" !steps_ok;
  (* Fix 3: a rep is timed on the device clock. A 5.0 s hold (device time
     from GO to the first sample below the disengage threshold), delivered in
     16-sample notifications 30 ms after the newest sample, with a 1.5 s
     radio stall in the middle: the delayed notifications arrive together and
     the stall recovery pushes the page mapping ahead (by ~0.9 s at the end
     of the rep), but the rep's seconds don't move. The MVP took the ending
     sample's page time minus GO: 5.87 s here, as the graph's shading still
     shows. *)
  let sm = sim (settings ()) in
  to_go sm;
  let go_page = sm.now and ts0 = sm.ts in
  let page_of ts = 1000. +. (ts /. 1000.) in
  let samples = Array.init 560 (fun j -> { Protocol.kg = (if float_of_int j *. 12.5 < 5000. then 20. else 0.); device_us = ts0 +. (float_of_int (j + 1) *. 12500.) }) in
  let arrivals = ref [] and last = ref 0. in
  for b = 0 to (560 / 16) - 1 do
    let batch = Array.sub samples (b * 16) 16 in
    let newest = page_of batch.(15).device_us in
    let at = if newest >= go_page +. 2000. && newest < go_page +. 3500. then go_page +. 3530. else newest +. 30. in
    let at = Float.max at !last in
    last := at;
    arrivals := (at, batch) :: !arrivals
  done;
  List.iter (fun (at, batch) -> ignore (feed sm (Session.Samples { now = at; samples = batch }))) (List.rev !arrivals);
  (match sm.st.current with
  | Some { reps = [ r ]; _ } -> check "stall mid-rep: rep timed on the device clock" (close ~eps:0.015 r.seconds 5.0)
  | _ -> check "stall mid-rep: one rep" false);
  let grip_end = List.find_map (function Session.Grip_ended e -> Some e | _ -> None) sm.events in
  check "stall mid-rep: the page mapping did jump"
    (match grip_end with Some e -> e -. (go_page /. 1000.) > 5.8 | None -> false);
  (* Fix 3: a stall straddling GO. GO is placed on the device clock from the
     earliest-arriving recent notification, so samples taken before GO stay
     out of the rep even though they arrive after it, and the rep is timed
     from GO: a pull from 0.3 s to 4.0 s after GO is a 4.0 s rep (plus the
     30 ms radio delay). Until 50 ms before GO the user rests a 7 kg hand
     on the gripper; those samples arrive after GO but are not in the rep. *)
  let sm = sim (settings ~countdown:3. ()) in
  let force page = if page >= 4300. && page < 8000. then 20. else if page >= 3500. && page < 3950. then 7. else 0. in
  let inputs = ref [ (1000., 0, Session.Start_set { now = 1000.; wall_ms = 0.; session_uuid = "u" }) ] in
  let t = ref 1100. in
  while !t <= 10000. do
    inputs := (!t, 1, Session.Tick { now = !t }) :: !inputs;
    t := !t +. 100.
  done;
  let last = ref 0. in
  for b = 0 to 44 do
    let batch = Array.init 16 (fun i -> let page = 1000. +. (float_of_int ((b * 16) + i + 1) *. 12.5) in { Protocol.kg = force page; device_us = (page -. 1000.) *. 1000. }) in
    let newest = 1000. +. (batch.(15).device_us /. 1000.) in
    let at = Float.max !last (if newest >= 3000. && newest < 4500. then 4830. else newest +. 30.) in
    last := at;
    inputs := (at, 2, Session.Samples { now = at; samples = batch }) :: !inputs
  done;
  List.iter (fun (_, _, i) -> ignore (feed sm i)) (List.stable_sort (fun (a, p, _) (b, q, _) -> compare (a, p) (b, q)) (List.rev !inputs));
  (match sm.st.current with
  | Some { reps = r :: _; _ } ->
      check "stall at GO: rep timed from GO" (close ~eps:0.015 r.seconds 4.03);
      (* 4.03 s at 80 Hz ≈ 323 samples, stored as means of 4. *)
      check "stall at GO: samples from before GO left out" (Array.length r.force_kg = 81)
  | _ -> check "stall at GO: a rep" false)

(* -------------------------------------------------------- JS numbers / JSON *)

let test_jsnum () =
  check "round ties up" (Jsnum.round 2.5 = 3. && Jsnum.round (-2.5) = -2. && Jsnum.round 0.49999999999999994 = 0.);
  check "r2" (Jsnum.r2 1.005 = 1. && Jsnum.r2 (-0.125) = -0.12 && Jsnum.r2 2.675 = 2.68);
  check "toFixed ties up" (Jsnum.to_fixed 0.25 1 = "0.3" && Jsnum.to_fixed 2.5 0 = "3" && Jsnum.to_fixed 1.005 2 = "1.00");
  check "toFixed negative zero" (Jsnum.to_fixed (-0.04) 1 = "-0.0" && Jsnum.to_fixed (-0.) 1 = "0.0");
  check "toFixed carry" (Jsnum.to_fixed 9.96 1 = "10.0" && Jsnum.to_fixed 99.5 0 = "100");
  let cases =
    [
      (0., "0");
      (-0., "0");
      (1., "1");
      (20.41168092460379, "20.41168092460379");
      (0.1, "0.1");
      (100., "100");
      (1e21, "1e+21");
      (1.5e-7, "1.5e-7");
      (1e-7, "1e-7");
      (1.2e-6, "0.0000012");
      (123456789012345680000., "123456789012345680000");
      (-2.5, "-2.5");
      (5e-324, "5e-324");
      (1.7976931348623157e308, "1.7976931348623157e+308");
    ]
  in
  List.iter (fun (x, s) -> check ("to_string " ^ s) (Jsnum.to_string x = s)) cases;
  check "uint32" (Jsnum.to_uint32 (-1.) = 4294967295. && Jsnum.to_uint32 4294967296. = 0. && Jsnum.to_uint32 12.7 = 12.);
  check "base36" (Jsnum.to_base36 1763890000000. = "mibikakg" && Jsnum.to_base36 35. = "z");
  check "iso" (Jsnum.iso_utc 1763890000123. = "2025-11-23T09:26:40+00:00" && Jsnum.iso_utc 0. = "1970-01-01T00:00:00+00:00");
  check "iso leap" (Jsnum.iso_utc 951782400000. = "2000-02-29T00:00:00+00:00");
  check "signed" (Jsnum.signed 1.25 1 = "+1.3" && Jsnum.signed (-0.) 1 = "+0.0" && Jsnum.signed (-2.) 2 = {js|−2.00|js})

let test_json () =
  let rt s = match Json.parse s with Ok j -> Json.to_string j | Error e -> "ERR " ^ e in
  check "roundtrip" (rt {|{"a":[1,2.5,-0,true,false,null],"b":{"c":"x\"y\\z\n\u0001"}}|} = {|{"a":[1,2.5,0,true,false,null],"b":{"c":"x\"y\\z\n\u0001"}}|});
  check "whitespace" (rt " [ 1 , { } , [ ] ] " = "[1,{},[]]");
  check "escapes" (rt {|"\/\b\f\r\t"|} = {|"/\b\f\r\t"|});
  check "unicode escape" (rt {|"é→😀"|} = "\"\u{e9}\u{2192}\u{1F600}\"");
  check "raw utf-8 passes through" (rt "\"\u{e9}\u{2192}\"" = "\"\u{e9}\u{2192}\"");
  check "numbers" (rt "[1e3,1E-2,0.000001,1e21,-12.5e+1]" = "[1000,0.01,0.000001,1e+21,-125]");
  List.iter
    (fun bad -> check ("rejects " ^ bad) (Result.is_error (Json.parse bad)))
    [ ""; "{"; "[1,]"; "01"; "1."; "+1"; "'x'"; "{\"a\" 1}"; "nul"; "\"\t\""; "[1] x"; ".5" ];
  check "indent"
    (Json.to_string ~indent:2 (Json.Object [ ("a", Json.Array [ Json.Number 1.; Json.Array [] ]); ("b", Json.Object []) ])
    = "{\n  \"a\": [\n    1,\n    []\n  ],\n  \"b\": {}\n}");
  check "duplicate keys: last wins" (Json.member "a" (Result.get_ok (Json.parse {|{"a":1,"a":2}|})) = Some (Json.Number 2.))

(* ------------------------------------------------------- settings, history *)

let test_settings () =
  check "defaults string"
    (Settings.to_string Settings.defaults
    = {|{"units":"lbs","windowS":5,"target":20.41168092460379,"sound":true,"countdownS":20,"restS":20,"engPct":90,"disPct":70,"tolPct":7.5,"gripper":"crusher","side":"left","renderDelayMs":300,"pcAuto":false,"plannedReps":4,"keepAwake":true,"pageZoom":85,"soundMode":"mix","volume":100}|});
  check "missing key" (Settings.of_string None = Settings.defaults);
  check "bad json" (Settings.of_string (Some "{") = Settings.defaults);
  let s =
    Settings.of_string
      (Some {|{"zzz":1,"windowS":7,"soundMode":"silent","volume":200,"pageZoom":20,"units":"kg","target":30,"plannedReps":6}|})
  in
  check "window fixed" (s.window_s = 5);
  check "sound mode fixed" (s.sound_mode = Settings.Mix);
  check "volume fixed" (s.volume = 100.);
  check "zoom fixed" (s.page_zoom = 85.);
  check "kg" (s.units = Settings.Kg && s.target = 30. && s.planned_reps = 6);
  check "extra kept last" (s.extra = [ ("zzz", Json.Number 1.) ]
    && String.ends_with ~suffix:{|"volume":100,"zzz":1}|} (Settings.to_string s));
  check "thresholds" (Settings.engage_kg (settings ()) = 10. && Settings.fail_kg (settings ()) = 4. && Settings.tolerance_kg (settings ()) = 1.);
  let d = Settings.defaults in
  check "reject dis >= eng" (Settings.set_number d Dis_pct 90. = None && Settings.set_number d Eng_pct 70. = None);
  check "reject out of range" (Settings.set_number d Rest_s 2. = None && Settings.set_number d Tol_pct Float.nan = None);
  check "accept" (Option.map (fun s -> s.Settings.rest_s) (Settings.set_number d Rest_s 45.) = Some 45.);
  let up = Settings.step_target d 1 in
  check "step target" (close ~eps:1e-9 (Settings.to_display up up.target) 46.);
  check "step target floor" (Settings.to_display d (Settings.step_target { d with target = 0.1 } (-1)).target = 1.);
  check "planned reps clamp" ((Settings.set_planned_reps d 25.).planned_reps = 20 && (Settings.set_planned_reps d 0.).planned_reps = 1);
  check "zoom step" ((Settings.step_zoom { d with page_zoom = 87. } 1).page_zoom = 90. && (Settings.step_zoom { d with page_zoom = 87. } (-1)).page_zoom = 85.);
  (* Fix 2 (UI): mid-set, what shapes the set is kept; the rest may change. *)
  let edited =
    {
      d with
      units = Kg;
      target = 33.;
      gripper = "micro";
      side = "right";
      planned_reps = 9;
      countdown_s = 3.;
      rest_s = 7.;
      eng_pct = 50.;
      dis_pct = 10.;
      tol_pct = 20.;
      sound = false;
      volume = 40.;
      page_zoom = 120.;
      window_s = 30;
      render_delay_ms = 0.;
      keep_awake = false;
      pc_auto = true;
      sound_mode = Over;
    }
  in
  let kept = Settings.keep_set_fields ~current:d edited in
  check "locked mid-set: set fields kept"
    (kept.units = d.units && kept.target = d.target && kept.gripper = d.gripper && kept.side = d.side
    && kept.planned_reps = d.planned_reps && kept.countdown_s = d.countdown_s && kept.rest_s = d.rest_s
    && kept.eng_pct = d.eng_pct && kept.dis_pct = d.dis_pct && kept.tol_pct = d.tol_pct);
  check "locked mid-set: the rest changes"
    (kept = { edited with units = d.units; target = d.target; gripper = d.gripper; side = d.side; planned_reps = d.planned_reps;
              countdown_s = d.countdown_s; rest_s = d.rest_s; eng_pct = d.eng_pct; dis_pct = d.dis_pct; tol_pct = d.tol_pct })

let test_history () =
  let sm = sim (settings ~reps:1 ()) in
  to_go sm;
  for _ = 1 to 100 do
    ignore (sample sm 20.)
  done;
  ignore (sample sm 0.);
  let set = match sm.st.current with None -> List.find_map (function Session.Set_ended s -> s | _ -> None) sm.events | s -> s in
  let set = Option.get set in
  let j = History.to_string [ set ] in
  check "history roundtrip" (History.to_string (History.of_string (Some j)) = j);
  check "non-array history" (History.of_string (Some "{}") = [] && History.of_string (Some "nope") = []);
  let with_pc =
    {
      set with
      pc =
        Some
          (History.Uploaded { at = "2026-01-01T00:00:00.000Z"; result = "created"; performance = Some { rank = None; prior_session_count = 7 } });
      extra = [ ("note", Json.String "x") ];
    }
  in
  let j2 = History.to_string [ with_pc ] in
  check "pc + extras roundtrip" (History.to_string (History.of_string (Some j2)) = j2);
  check "upsert replaces" (List.length (History.upsert [ set; with_pc ] with_pc) = 2);
  check "upsert appends" (List.length (History.upsert [] set) = 1);
  check "remove" (History.remove [ set ] [ set.id ] = []);
  let re = History.relog set 21. in
  check "relog" (re.target_kg = 21. && (List.hd re.reps).dev_kg = Jsnum.r2 ((List.hd re.reps).median_kg -. 21.));
  check "relog same" (History.relog set (set.target_kg +. 1e-7) == set);
  check "export"
    (History.export_json [ set ]
    = "{\n  \"date_time\": \"2023-11-14T22:13:20+00:00\",\n  \"gripper\": \"crusher\",\n  \"side\": \"left\",\n  \"weight_lbs\": 44.1,\n  \"reps\": [\n    1\n  ]\n}");
  check "pc payload"
    (Json.to_string (History.pc_payload set ~session_id:"new")
    = {|{"source_session_id":"u","date_time":"2023-11-14T22:13:20+00:00","gripper":"crusher","side":"left","weight_lbs":44.1,"reps":[1],"excluded_from_metrics":false}|});
  check "token" (History.valid_pc_token ("pcgg_" ^ String.make 64 'a') && not (History.valid_pc_token ("pcgg_" ^ String.make 64 'A')));
  check "pc 401" (match History.pc_result ~status:(Some 401) ~body:(Json.Object []) ~at:"" with Failed _ -> true | _ -> false);
  check "pc error text"
    (History.pc_result ~status:(Some 500) ~body:(Json.Object [ ("error", Json.String "boom") ]) ~at:"a"
    = Failed { at = "a"; message = "boom" });
  check "pc uploaded"
    (History.pc_result ~status:(Some 200)
       ~body:(Result.get_ok (Json.parse {|{"status":"merged","performance":{"rank":0.8,"prior_session_count":9}}|}))
       ~at:"a"
    = Uploaded { at = "a"; result = "merged"; performance = Some { rank = Some 0.8; prior_session_count = 9 } });
  check "rank text"
    (History.rank_text (Some { rank = Some 0.8; prior_session_count = 9 }) set
    = {js|Rank 0.80 · Excellent, against 9 earlier Crusher Left sets.|js});
  (* Fix 7: no reply leaves a set "uploading". The MVP threw on a `null`
     body and kept {state: "uploading"} for good (and its retry refused to
     start while it said so). *)
  let unrecognised = History.Failed { at = "a"; message = "PowerCurve sent a reply this page doesn't recognise." } in
  List.iter
    (fun (name, body) ->
      check ("pc 200 with a " ^ name ^ " body fails, retryable") (History.pc_result ~status:(Some 200) ~body ~at:"a" = unrecognised);
      check ("pc 500 with a " ^ name ^ " body fails")
        (History.pc_result ~status:(Some 500) ~body ~at:"a"
        = Failed { at = "a"; message = "PowerCurve couldn't save this set (error 500)." }))
    [ ("null", Json.Null); ("array", Json.Array [ Json.Number 1. ]); ("string", Json.String "created"); ("number", Json.Number 3.) ];
  check "pc uploading while in flight" (History.pc_shown (Some Uploading) ~in_flight:true = Some Uploading);
  check "pc uploading left behind reads as failed"
    (History.pc_shown (Some Uploading) ~in_flight:false = Some (Failed { at = ""; message = History.interrupted_message }));
  check "pc other states as stored"
    (History.pc_shown None ~in_flight:false = None
    && History.pc_shown (Some unrecognised) ~in_flight:false = Some unrecognised);
  (* The old version's history can hold "uploading": it loads as such and shows as failed. *)
  (match History.of_string (Some {|[{"id":"x","date_time":"d","gripper":"micro","side":"left","target_kg":10,"planned_reps":1,"countdown_s":3,"rest_s":3,"rep_end_after_pct":90,"rep_end_below_pct":70,"tolerance_pct":7.5,"reps":[],"pc":{"state":"uploading"}}]|}) with
  | [ s ] -> check "pc uploading from storage shows as failed" (History.pc_shown s.pc ~in_flight:false <> Some Uploading)
  | _ -> check "pc uploading from storage" false)

(* --------------------------------------------------------- buffer, graph *)

let test_buffer_graph () =
  let b = Force_buffer.create () in
  for i = 0 to 80 * 700 - 1 do
    Force_buffer.push b (float_of_int i /. 80.) 1.
  done;
  let t0 = Force_buffer.time b 0 and tl = Option.get (Force_buffer.last_time b) in
  check "trimmed to ~600 s" (tl -. t0 <= 605. && tl -. t0 >= 600.);
  Force_buffer.start_grip b 10.;
  Force_buffer.end_grip b 20.;
  Force_buffer.start_grip b 30.;
  Force_buffer.discard_grip b;
  check "grips" (List.map (fun (g : Force_buffer.grip) -> (g.start, g.stop)) (Force_buffer.grips b) = [ (10., Some 20.) ]);
  let b = Force_buffer.create () in
  Force_buffer.push b 1. 10.;
  Force_buffer.push b 2. 20.;
  check "value_at" (Force_buffer.value_at b 1.5 = 15. && Force_buffer.value_at b 0. = 10. && Force_buffer.value_at b 9. = 20.);
  check "nice step" (Graph.nice_step 40. = 10. && Graph.nice_step 9. = 2.5 && Graph.nice_step 0.4 = 0.1);
  let s = settings ~target:20. ~tol:5. () in
  let lo, hi = Graph.y_domain { s with units = Kg } Float.infinity Float.neg_infinity in
  check "y domain centred" (lo = 0. && hi = 40.);
  let lo, hi = Graph.y_domain { s with units = Kg } 19.5 20.5 in
  check "y domain min band" (lo = 19. && hi = 21.);
  let g = Graph.layout s (Force_buffer.create ()) ~x1:10. ~width:360. ~height:210. in
  check "empty graph" (g.empty && g.line = [] && List.length g.x_labels = 6);
  let b = Force_buffer.create () in
  for i = 0 to 80 * 40 do
    Force_buffer.push b (float_of_int i /. 80.) (if i mod 2 = 0 then 10. else 30.)
  done;
  let g = Graph.layout { s with window_s = 30 } b ~x1:40. ~width:200. ~height:210. in
  check "decimated to 2 points per column" (List.length g.line = (2 * 237) + 1);
  check "peaks kept"
    (List.exists (fun (_, y) -> y = Graph.(g.plot.y +. g.plot.h -. ((30. *. Settings.kg_to_lbs -. g.y0) /. (g.y1 -. g.y0) *. g.plot.h))) g.line);
  check "render time" (Graph.render_time ~streaming:true ~now:1000. ~render_delay_ms:300. ~frozen:None ~last_sample:None = (0.7, Some 0.7));
  check "sound off" (Sound.tones { s with sound = false } Session.Rep_started = []);
  check "set complete tones" (List.length (Sound.tones s Session.Set_complete) = 2)

let () =
  section "statistics" test_stats;
  section "reps" test_reps;
  section "protocol" test_protocol;
  section "thresholds" test_thresholds;
  section "off target" test_off_target;
  section "rep flow" test_rep_flow;
  section "end rep" test_end_rep;
  section "weighing" test_weighing;
  section "clock" test_clock;
  section "js numbers" test_jsnum;
  section "json" test_json;
  section "settings" test_settings;
  section "history" test_history;
  section "buffer + graph" test_buffer_graph;
  Printf.printf "%d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
