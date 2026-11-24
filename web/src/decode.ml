let field name decode json =
  match Js.Json.decodeObject json with
  | None -> None
  | Some o -> Option.bind (Js.Dict.get o name) decode

let string_field name = field name Js.Json.decodeString
let number_field name = field name Js.Json.decodeNumber
let bool_field name = field name Js.Json.decodeBoolean

let int_field name json =
  Option.bind (number_field name json) (fun x ->
      if Float.is_integer x then Some (int_of_float x) else None)

let nullable name decode json =
  match Js.Json.decodeObject json with
  | None -> None
  | Some o -> (
      match Js.Dict.get o name with
      | None -> None
      | Some v when Js.Json.test v Js.Json.Null -> Some None
      | Some v -> Option.map Option.some (decode v))

let array decode json =
  Option.bind (Js.Json.decodeArray json) (fun items ->
      let decoded = Array.map decode items in
      if Array.for_all Option.is_some decoded then
        Some (Array.map Option.get decoded)
      else None)
