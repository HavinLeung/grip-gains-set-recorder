(* Checks of the Timer's pure conversions, run in Node against this module's Melange output by
   tests/unit/web_checks.test.mjs (not part of the app bundle: nothing in the app imports it).

   [run ()] returns the failures (empty when everything holds). *)

module S = Timer_core.Settings

let failures = ref []
let check name ok = if not ok then failures := name :: !failures

let set_types () =
  let label h = Option.map Set_types.label (Set_types.classify h) in
  check "47.9 s is power" (Set_types.classify 47.9 = Some Power);
  check "48 s is power/strength" (Set_types.classify 48. = Some Power_strength);
  check "81.9 s is power/strength"
    (Set_types.classify 81.9 = Some Power_strength);
  check "82 s is strength" (Set_types.classify 82. = Some Strength);
  check "128.9 s is strength" (Set_types.classify 128.9 = Some Strength);
  check "129 s is strength/endurance"
    (Set_types.classify 129. = Some Strength_endurance);
  check "179.9 s is strength/endurance"
    (Set_types.classify 179.9 = Some Strength_endurance);
  check "180 s is endurance" (Set_types.classify 180. = Some Endurance);
  check "label: 12 s is Power" (label 12. = Some "Power");
  check "label: 400 s is Endurance" (label 400. = Some "Endurance");
  check "not a time has no type"
    (Set_types.classify nan = None && Set_types.classify (-1.) = None)

let summary () =
  let open Timer_core in
  let rep seconds median =
    {
      (Rep.make ~duration:seconds
         [| median; median; median; median |]
         ~target_kg:20.)
      with
      seconds;
    }
  in
  let set : History.set =
    {
      id = Jsnum.to_base36 1795425334110.;
      source_session_id = Some "0b6c3f0e-5b7e-4a83-9d1c-2f6d8c1e4a10";
      date_time = Jsnum.iso_utc 1795425334110.;
      gripper = "micro";
      side = "left";
      target_kg = 45. /. S.kg_to_lbs;
      planned_reps = 3;
      countdown_s = 20.;
      rest_s = 20.;
      rep_end_after_pct = 90.;
      rep_end_below_pct = 70.;
      tolerance_pct = 7.5;
      reps = [ rep 35.27 20.5; rep 30.1 20.4; rep 0. 20. ];
      pc = None;
      extra = [];
    }
  in
  let b = Timer_summary.of_set ~manual:false set in
  let num k = Decode.number_field k b in
  check "the id is the session uuid"
    (Decode.string_field "id" b = Some "0b6c3f0e-5b7e-4a83-9d1c-2f6d8c1e4a10");
  check "performed_at from the base-36 id"
    (num "performed_at" = Some 1795425334110.);
  check "weight_lbs: kg target back to lbs, 0.1 lb" (num "weight_lbs" = Some 45.);
  check "planned_reps" (num "planned_reps" = Some 3.);
  (match Decode.field "rep_seconds" (Decode.array Js.Json.decodeNumber) b with
  | Some [| a; b'; c |] ->
      check "rep seconds (a 0 s rep is logged as 0.01 s)"
        (a = 35.27 && b' = 30.1 && c = 0.01)
  | _ -> check "rep_seconds has three holds" false);
  (match Decode.field "rep_stats" (Decode.array Option.some) b with
  | Some [| r1; _; _ |] ->
      check "rep stats in lbs, 2 decimals"
        (Decode.number_field "median_lbs" r1
        = Some (Jsnum.r2 (20.5 *. S.kg_to_lbs)))
  | _ -> check "rep_stats has three entries" false);
  check "set type from rep 1 (35 s: power)"
    (Decode.string_field "set_type" b = Some "power");
  let long = { set with reps = [ rep 400. 20. ] } in
  let bl = Timer_summary.of_set ~manual:false long in
  check "over 300 s is endurance"
    (Decode.nullable "set_type" Js.Json.decodeString bl
    = Some (Some "endurance"));
  let short = { set with reps = [ rep 12. 20. ] } in
  let bs = Timer_summary.of_set ~manual:false short in
  check "under 20 s is power"
    (Decode.nullable "set_type" Js.Json.decodeString bs = Some (Some "power"));
  check "base36 round trip"
    (Timer_summary.ms_of_base36 (Jsnum.to_base36 1700000000123.)
    = Some 1700000000123.);
  (* export: Copy JSON's shape *)
  let sm = Option.get (Timer_summary.of_json b) in
  let j = Timer_summary.export_json [ Timer_summary.export_obj sm ] in
  check "export shape"
    (j
    = "{\n  \"date_time\": \""
      ^ Jsnum.iso_utc 1795425334110.
      ^ "\",\n\
        \  \"gripper\": \"micro\",\n\
        \  \"side\": \"left\",\n\
        \  \"weight_lbs\": 45,\n\
        \  \"reps\": [\n\
        \    35,\n\
        \    30,\n\
        \    0\n\
        \  ]\n\
         }")

(* Manual mode: a set ended by taps only (the core's reps have no samples: zero statistics). *)
let manual_body () =
  let open Timer_core in
  let rep seconds =
    Rep.make ~duration:seconds [||] ~target_kg:(24. /. S.kg_to_lbs)
  in
  let set : History.set =
    {
      id = Jsnum.to_base36 1795425334110.;
      source_session_id = Some "6f1c3f0e-5b7e-4a83-9d1c-2f6d8c1e4a10";
      date_time = Jsnum.iso_utc 1795425334110.;
      gripper = "micro";
      side = "left";
      target_kg = 24. /. S.kg_to_lbs;
      planned_reps = 3;
      countdown_s = 10.;
      rest_s = 20.;
      rep_end_after_pct = 90.;
      rep_end_below_pct = 70.;
      tolerance_pct = 7.5;
      reps = [ rep 104.25; rep 61.5; rep 40.04 ];
      pc = None;
      extra = [];
    }
  in
  let b = Timer_summary.of_set ~manual:true set in
  check "manual: the summary says so" (Decode.bool_field "manual" b = Some true);
  check "manual: rep_stats is null, not zeros"
    (Decode.nullable "rep_stats" Option.some b = Some None);
  check "manual: weight is the target"
    (Decode.number_field "weight_lbs" b = Some 24.);
  (match Decode.field "rep_seconds" (Decode.array Js.Json.decodeNumber) b with
  | Some [| a; b'; c |] ->
      check "manual: hold times" (a = 104.25 && b' = 61.5 && c = 40.04)
  | _ -> check "manual: three holds" false);
  check "manual: set type from rep 1 (104 s: strength)"
    (Decode.string_field "set_type" b = Some "strength");
  let sm = Option.get (Timer_summary.of_json b) in
  check "manual: read back as manual" sm.manual;
  (* a normal set's summary is not manual *)
  let nb = Timer_summary.of_set ~manual:false set in
  check "normal: the summary says not manual"
    (Decode.bool_field "manual" nb = Some false);
  check "normal: rep_stats present"
    (match Decode.nullable "rep_stats" Option.some nb with
    | Some (Some _) -> true
    | _ -> false);
  check "normal: summary not manual"
    (not (Option.get (Timer_summary.of_json nb)).manual);
  (* rebuilt from the record: the PowerCurve payload is the original's *)
  let back = Timer_summary.manual_set sm ~pc:None in
  check "manual: same id"
    (Timer_summary.id_of_set back = Timer_summary.id_of_set set);
  check "manual: same PowerCurve payload"
    (Json.to_string (History.pc_payload back ~session_id:"")
    = Json.to_string (History.pc_payload set ~session_id:""));
  (* the upload state kept in the record reads back as it was *)
  let pc =
    History.Uploaded
      {
        at = "2026-11-23T10:00:00.000Z";
        result = "created";
        performance = Some { rank = Some 0.8; prior_session_count = 12 };
      }
  in
  check "manual: PowerCurve state round trip"
    (Timer_summary.pc_of_string (Timer_summary.pc_to_string pc) = Some pc);
  check "manual: failed state round trip"
    (let f = History.Failed { at = "x"; message = "m" } in
     Timer_summary.pc_of_string (Timer_summary.pc_to_string f) = Some f)

let run () =
  failures := [];
  (try manual_body ()
   with e -> check ("manual body raised " ^ Printexc.to_string e) false);
  (try set_types ()
   with e -> check ("set types raised " ^ Printexc.to_string e) false);
  (try summary ()
   with e -> check ("summary raised " ^ Printexc.to_string e) false);
  Array.of_list (List.rev !failures)
