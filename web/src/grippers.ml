(* The grippers a set can be recorded on: the identifier kept with the set, and the name
   shown. Every gripper is used one hand at a time (left or right). *)

let all = [ ("crusher", "Crusher"); ("micro", "Micro") ]
let sides = [ "left"; "right" ]

(* A set saved under an identifier that is no longer listed still shows, capitalised. *)
let name id =
  match List.assoc_opt id all with
  | Some name -> name
  | None -> Timer_core.Jsnum.capitalize id
