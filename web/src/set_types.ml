(* Set types, by the rep-1 hold in whole seconds (floored: 47.9 s is power, 48.0 s
   power/strength):

     power                 up to 47 s
     power / strength      48-81 s
     strength              82-128 s
     strength / endurance  129-179 s
     endurance             180 s and over

   Here they are only a label on a finished set (the review, the set sheet). *)

type t = Power | Power_strength | Strength | Strength_endurance | Endurance

(* The identifier kept in a saved set's summary. *)
let id = function
  | Power -> "power"
  | Power_strength -> "power_strength"
  | Strength -> "strength"
  | Strength_endurance -> "strength_endurance"
  | Endurance -> "endurance"

let label = function
  | Power -> "Power"
  | Power_strength -> "Power / strength"
  | Strength -> "Strength"
  | Strength_endurance -> "Strength / endurance"
  | Endurance -> "Endurance"

(* The type of a set with this rep-1 hold; [None] only for a hold that isn't a time. *)
let classify hold =
  if not (Float.is_finite hold && hold >= 0.) then None
  else
    Some
      (if hold < 48. then Power
       else if hold < 82. then Power_strength
       else if hold < 129. then Strength
       else if hold < 180. then Strength_endurance
       else Endurance)
