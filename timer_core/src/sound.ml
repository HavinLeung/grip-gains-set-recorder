type tone = { freq : float; duration : float; delay_ms : float }

let t ?(delay_ms = 0.) freq duration = { freq; duration; delay_ms }

let tones (s : Settings.t) (e : Session.event) =
  if not s.sound then []
  else
    match e with
    | Countdown_beep 1 -> [ t 1174.66 0.24 ]
    | Countdown_beep 2 -> [ t 660. 0.18 ]
    | Countdown_beep _ -> [ t 880. 0.12 ]
    | Rep_started -> [ t 1320. 0.30 ]
    | Rest_started -> [ t 440. 0.3 ]
    | Set_complete -> [ t 660. 0.15; t ~delay_ms:180. 1320. 0.3 ]
    | Off_target_alert { heavy; _ } -> [ t (if heavy then 1320. else 440.) 0.15 ]
    | _ -> []

let peak_gain (s : Settings.t) = 0.35 *. (s.volume /. 100.)
let test_tones = [ t 880. 0.12; t ~delay_ms:250. 1320. 0.15; t ~delay_ms:500. 440. 0.15 ]
let volume_preview = t 880. 0.15
