(* Small formatting helpers shared by the Timer's components (the MVP's fmt, signed, cap, day
   labels). Everything is kg inside; Settings converts for display. *)

open Timer_core

let fmt ?(digits = 1) (s : Settings.t) kg = Settings.format ~digits s kg
let unit_ (s : Settings.t) = Settings.unit_label s

(* +(x).toFixed(1): the shortest form of a 1-decimal value ("45" not "45.0"). *)
let short1 x =
  Js.Float.toString (float_of_string (Js.Float.toFixed ~digits:1 x))

let disp1 (s : Settings.t) kg = short1 (Settings.to_display s kg)
let signed v d = Jsnum.signed v d
let plural n word = string_of_int n ^ " " ^ word ^ if n = 1 then "" else "s"
let secs1 x = Js.Float.toFixed ~digits:1 x ^ "s"

(* Local calendar day of an instant, and the MVP's day headings: Today, Yesterday, "Mon, Nov 21"
   (with the year when it isn't this year). *)
external to_locale_date_string :
  Js.Date.t -> string Js.undefined -> < .. > Js.t -> string
  = "toLocaleDateString"
[@@mel.send]

external to_locale_string :
  Js.Date.t -> string Js.undefined -> < .. > Js.t -> string = "toLocaleString"
[@@mel.send]

external to_date_string : Js.Date.t -> string = "toDateString" [@@mel.send]

let day_key ms = to_date_string (Js.Date.fromFloat ms)

let day_label ms =
  let d = Js.Date.fromFloat ms in
  let today = Js.Date.make () in
  let yesterday = Js.Date.fromFloat (Js.Date.getTime today -. 86_400_000.) in
  if to_date_string d = to_date_string today then "Today"
  else if to_date_string d = to_date_string yesterday then "Yesterday"
  else if Js.Date.getFullYear d = Js.Date.getFullYear today then
    to_locale_date_string d Js.undefined
      [%mel.obj { weekday = "short"; month = "short"; day = "numeric" }]
  else
    to_locale_date_string d Js.undefined
      [%mel.obj
        {
          weekday = "short";
          month = "short";
          day = "numeric";
          year = "numeric";
        }]

let when_label ms =
  to_locale_string (Js.Date.fromFloat ms) Js.undefined
    [%mel.obj
      {
        weekday = "short";
        month = "short";
        day = "numeric";
        hour = "numeric";
        minute = "2-digit";
      }]

(* A set's side as the Timer shows it: "Left" or "Right". *)
let side_label = Jsnum.capitalize
