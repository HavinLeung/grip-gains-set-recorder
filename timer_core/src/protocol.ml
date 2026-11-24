open! Fp

let service_uuid = "7e4e1701-1ea6-40c9-9dcc-13d34ffead57"
let notify_uuid = "7e4e1702-1ea6-40c9-9dcc-13d34ffead57"
let write_uuid = "7e4e1703-1ea6-40c9-9dcc-13d34ffead57"

type command = Tare | Start_measuring | Shutdown | Battery

let command_byte = function Tare -> 0x64 | Start_measuring -> 0x65 | Shutdown -> 0x6E | Battery -> 0x6F
let connect_commands = [ Battery; Start_measuring ]

type view = { length : int; get_u8 : int -> int }

let view_of_array a = { length = Array.length a; get_u8 = (fun i -> a.(i) land 0xFF) }
let view_of_string s = { length = String.length s; get_u8 = (fun i -> Char.code s.[i] land 0xFF) }

type sample = { kg : float; device_us : float }
type packet = Battery_mv of float | Samples of sample array | Ignored

(* pow2.(k + 150) = 2^k exactly, for k in [-150, 105]; built by repeated
   doubling so it does not depend on any pow implementation. *)
let pow2 =
  let t = Array.make 256 0. in
  t.(150) <- 1.;
  for k = 151 to 255 do
    t.(k) <- t.(k - 1) *. 2.
  done;
  for k = 149 downto 0 do
    t.(k) <- t.(k + 1) *. 0.5
  done;
  t

let uint32_le v o =
  float_of_int (v.get_u8 o)
  +. (float_of_int (v.get_u8 (o + 1)) *. 256.)
  +. (float_of_int (v.get_u8 (o + 2)) *. 65536.)
  +. (float_of_int (v.get_u8 (o + 3)) *. 16777216.)

let float32_le v o =
  let b0 = v.get_u8 o and b1 = v.get_u8 (o + 1) and b2 = v.get_u8 (o + 2) and b3 = v.get_u8 (o + 3) in
  let negative = b3 land 0x80 <> 0 in
  let exp = ((b3 land 0x7F) lsl 1) lor (b2 lsr 7) in
  let mant = ((b2 land 0x7F) lsl 16) lor (b1 lsl 8) lor b0 in
  let mag =
    if exp = 0xFF then if mant = 0 then Float.infinity else Float.nan
    else if exp = 0 then float_of_int mant *. pow2.(150 - 149)
    else float_of_int (mant lor 0x800000) *. pow2.(exp - 150 + 150)
  in
  if negative then -.mag else mag

let parse ~awaiting_battery v =
  if awaiting_battery && v.length >= 6 && v.get_u8 0 = 0 && v.get_u8 1 = 4 then Battery_mv (uint32_le v 2)
  else if v.length < 6 || v.get_u8 0 <> 1 then Ignored
  else
    let n = (v.length - 2) / 8 in
    Samples (Array.init n (fun k -> { kg = float32_le v (2 + (8 * k)); device_us = uint32_le v (2 + (8 * k) + 4) }))

let li_curve =
  [|
    (4200., 100);
    (4100., 90);
    (4000., 80);
    (3920., 70);
    (3870., 60);
    (3820., 50);
    (3790., 40);
    (3770., 30);
    (3740., 20);
    (3680., 10);
    (3450., 5);
    (3300., 0);
  |]

let battery_percent mv =
  let n = Array.length li_curve in
  let rec go i =
    if i >= n - 1 then 0
    else
      let umv, up = li_curve.(i) and lmv, lp = li_curve.(i + 1) in
      if mv >= umv then up
      else if mv >= lmv then
        let lp = float_of_int lp and up = float_of_int up in
        int_of_float (Jsnum.round (lp +. ((mv -. lmv) /. (umv -. lmv) *. (up -. lp))))
      else go (i + 1)
  in
  go 0
