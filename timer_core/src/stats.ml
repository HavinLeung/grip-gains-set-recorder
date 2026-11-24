open! Fp

let sum a = Array.fold_left ( +. ) 0. a
let mean a = if Array.length a = 0 then 0. else sum a /. float_of_int (Array.length a)

let sorted a =
  let s = Array.copy a in
  Array.stable_sort Float.compare s;
  s

let median a =
  let n = Array.length a in
  if n = 0 then 0.
  else
    let s = sorted a and m = n asr 1 in
    if n mod 2 = 1 then s.(m) else (s.(m - 1) +. s.(m)) /. 2.

let sq_dev a =
  let m = mean a in
  Array.fold_left (fun acc x -> acc +. ((x -. m) *. (x -. m))) 0. a

let sd a =
  let n = Array.length a in
  if n < 2 then 0. else Float.sqrt (sq_dev a /. float_of_int (n - 1))

let pop_sd a =
  let n = Array.length a in
  if n = 0 then 0. else Float.sqrt (sq_dev a /. float_of_int n)

let percentile_sorted s p =
  let n = Array.length s in
  if n = 0 then 0.
  else
    let i = p *. float_of_int (n - 1) in
    let lo = int_of_float (Float.floor i) in
    let hi = min (lo + 1) (n - 1) in
    s.(lo) +. ((i -. float_of_int lo) *. (s.(hi) -. s.(lo)))

let trimmed_median ?(fraction = 0.3) a =
  let n = Array.length a in
  if n < 5 then median a
  else
    let t = int_of_float (Float.floor (float_of_int n *. fraction)) in
    if t < n - t then median (Array.sub a t (n - (2 * t))) else median a

module Running = struct
  type t = { n : int; m : float; q : float }

  let empty = { n = 0; m = 0.; q = 0. }

  let push r x =
    let n = r.n + 1 in
    let d = x -. r.m in
    let m = r.m +. (d /. float_of_int n) in
    { n; m; q = r.q +. (d *. (x -. m)) }

  let mean r = r.m
  let sd r = if r.n > 1 then Float.sqrt (r.q /. float_of_int (r.n - 1)) else 0.
end
