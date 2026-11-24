(* Manual mode (no Progressor: every rep ends with End rep), the one Timer choice the core's
   settings don't have. It is kept in the same stored settings object, which carries keys the
   core doesn't know in [extra], under "manualMode". This module is the only place that knows
   the key. *)

open Timer_core

let key = "manualMode"

let manual (s : Settings.t) =
  Option.value ~default:false
    (Option.bind (List.assoc_opt key s.extra) Json.to_bool)

let with_manual (s : Settings.t) on =
  {
    s with
    extra =
      List.filter (fun (k, _) -> k <> key) s.extra @ [ (key, Json.Bool on) ];
  }
