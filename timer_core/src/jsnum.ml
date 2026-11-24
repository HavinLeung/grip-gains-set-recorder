open! Fp

let round x =
  let f = Float.floor x in
  if x -. f >= 0.5 then f +. 1. else f

let r2 x = round (x *. 100.) /. 100.
let two32 = 4294967296.

let to_uint32 x =
  if not (Float.is_finite x) then 0.
  else
    let i = if x < 0. then -.Float.floor (-.x) else Float.floor x in
    let m = Float.rem i two32 in
    if m < 0. then m +. two32 else m +. 0.

(* The exact decimal expansion of a positive finite double, without Printf (which would add
   ~40 KiB to the web bundle). A double is [m · 2^e] with [m < 2^53]; its value is the integer
   [m · 2^e] when [e ≥ 0], and [m · 5^-e / 10^-e] otherwise, so it has a finite decimal
   expansion. Big naturals are little-endian arrays of base-10^4 limbs (products stay well below
   2^31, so this is exact under Melange's 32-bit ints too). Returns all the significant digits and
   [k], the number of them after the decimal point: value = digits · 10^-k. *)
let limb = 10000

let big_of_float m =
  (* m: a non-negative integer below 2^53 *)
  (* exact: m − (m mod 10^4) is a multiple of 10^4 below 2^53 *)
  let rec go m acc = if m = 0. then acc else go ((m -. Float.rem m 1e4) /. 1e4) (int_of_float (Float.rem m 1e4) :: acc) in
  Array.of_list (List.rev (go m []))

let big_mul_small a k =
  let n = Array.length a in
  let out = Array.make (n + 2) 0 in
  let carry = ref 0 in
  for i = 0 to n - 1 do
    let v = (a.(i) * k) + !carry in
    out.(i) <- v mod limb;
    carry := v / limb
  done;
  let i = ref n in
  while !carry > 0 do
    out.(!i) <- !carry mod limb;
    carry := !carry / limb;
    incr i
  done;
  Array.sub out 0 !i

let big_to_digits a =
  let n = Array.length a in
  if n = 0 then "0"
  else begin
    let b = Buffer.create (4 * n) in
    Buffer.add_string b (string_of_int a.(n - 1));
    for i = n - 2 downto 0 do
      let d = string_of_int a.(i) in
      Buffer.add_string b (String.make (4 - String.length d) '0');
      Buffer.add_string b d
    done;
    Buffer.contents b
  end

(* x = m · 2^e with m an integer in [2^52, 2^53): scale by exact powers of two (every double in
   that range is an integer). Not Float.frexp: Melange's is computed with logarithms and is
   neither exact nor defined for subnormals. *)
let decompose x =
  let m = ref x and e = ref 0 in
  while !m >= 9007199254740992. do
    m := !m /. 2.;
    incr e
  done;
  while !m < 4503599627370496. do
    m := !m *. 2.;
    decr e
  done;
  (!m, !e)

let exact_digits x =
  if x = 0. then ("0", 0) else
  let m, e = decompose x in
  let a = ref (big_of_float m) in
  if e >= 0 then begin
    let r = ref e in
    while !r >= 13 do
      a := big_mul_small !a 8192;
      r := !r - 13
    done;
    a := big_mul_small !a (1 lsl !r);
    (big_to_digits !a, 0)
  end
  else begin
    let k = -e in
    let r = ref k in
    while !r >= 6 do
      a := big_mul_small !a 15625;
      r := !r - 6
    done;
    for _ = 1 to !r do
      a := big_mul_small !a 5
    done;
    (big_to_digits !a, k)
  end

(* Rounds the digit string [d] (an exact value) to its first [p] characters, ties to even, as
   libc's printf does; returns the digits and whether a carry added a leading digit. *)
let round_exact d p =
  if String.length d <= p then (d ^ String.make (p - String.length d) '0', false)
  else begin
    let head = String.sub d 0 p in
    let up =
      match d.[p] with
      | '6' .. '9' -> true
      | '5' ->
          let rest_zero = ref true in
          for i = p + 1 to String.length d - 1 do
            if d.[i] <> '0' then rest_zero := false
          done;
          (not !rest_zero) || (p > 0 && (Char.code head.[p - 1] - 48) mod 2 = 1)
      | _ -> false
    in
    if not up then (head, false)
    else
      let rec bump i acc =
        if i < 0 then ("1" ^ acc, true)
        else if head.[i] = '9' then bump (i - 1) ("0" ^ acc)
        else (String.sub head 0 i ^ String.make 1 (Char.chr (Char.code head.[i] + 1)) ^ acc, false)
      in
      bump (p - 1) ""
  end

(* What [Printf.sprintf "%.60e" x] gives, as (61 digits, decimal exponent). *)
let exp60 x =
  let d, k = exact_digits x in
  let e = String.length d - 1 - k in
  let r, carry = round_exact d 61 in
  if carry then (String.sub r 0 61, e + 1) else (r, e)

(* What [Printf.sprintf "%.60f" x] gives (x ≥ 0): integer part, "." and 60 decimals. *)
let fixed60 x =
  let d, k = exact_digits x in
  (* value = d · 10^-k; write it with exactly max(k, 60) decimals, then round to 60 *)
  let d = if String.length d <= k then String.make (k - String.length d + 1) '0' ^ d else d in
  let int_len = String.length d - k in
  let all, carry = round_exact d (int_len + 60) in
  let int_len = if carry then int_len + 1 else int_len in
  String.sub all 0 int_len ^ "." ^ String.sub all int_len 60

let strip_zeros d =
  let k = ref (String.length d) in
  while !k > 1 && d.[!k - 1] = '0' do
    decr k
  done;
  String.sub d 0 !k

(* Adds one unit in the last place of a string of decimal digits. *)
let incr_digits s =
  let b = Bytes.of_string s in
  let rec go i =
    if i < 0 then true
    else if Bytes.get b i = '9' then begin
      Bytes.set b i '0';
      go (i - 1)
    end
    else begin
      Bytes.set b i (Char.chr (Char.code (Bytes.get b i) + 1));
      false
    end
  in
  let carry = go (Bytes.length b - 1) in
  (if carry then "1" else "") ^ Bytes.to_string b

(* Rounds a digit string (exact, from "%.60e") to [p] digits, ties to even;
   returns the digits and how much the exponent grew (0 or 1). *)
let round_digits d p =
  let head = String.sub d 0 p in
  let rest = String.sub d p (String.length d - p) in
  let up =
    match rest.[0] with
    | '6' .. '9' -> true
    | '5' ->
        let tail_zero = ref true in
        String.iteri (fun i c -> if i > 0 && c <> '0' then tail_zero := false) rest;
        (not !tail_zero) || (Char.code head.[p - 1] - 48) mod 2 = 1
    | _ -> false
  in
  if not up then (head, 0)
  else
    let r = incr_digits head in
    if String.length r > p then (String.sub r 0 p, 1) else (r, 0)

let shortest x =
  (* The exact decimal expansion (both libc and JS's toExponential print the
     exact binary value), rounded to the fewest digits that read back as [x].
     Rounding here, ties to even like Number.prototype.toString, rather than
     with "%.Ne": JS's toExponential breaks ties upward, libc to even. *)
  let digits, e = exp60 x in
  let cand p =
    let d, carry = round_digits digits p in
    (d, e + carry)
  in
  let reads_back (d, e) =
    float_of_string (String.sub d 0 1 ^ "." ^ String.sub d 1 (String.length d - 1) ^ "e" ^ string_of_int e) = x
  in
  (* Round-tripping is monotonic in the digit count, so binary search 1..17. *)
  let rec go lo hi =
    if lo >= hi then lo
    else
      let mid = (lo + hi) / 2 in
      if reads_back (cand mid) then go lo mid else go (mid + 1) hi
  in
  let d, e = cand (go 1 17) in
  (strip_zeros d, e + 1)

let rec to_string x =
  if Float.is_nan x then "NaN"
  else if x = Float.infinity then "Infinity"
  else if x = Float.neg_infinity then "-Infinity"
  else if x = 0. then "0"
  else if x < 0. then "-" ^ to_string (-.x)
  else
    let d, n = shortest x in
    let k = String.length d in
    if k <= n && n <= 21 then d ^ String.make (n - k) '0'
    else if 0 < n && n <= 21 then String.sub d 0 n ^ "." ^ String.sub d n (k - n)
    else if -6 < n && n <= 0 then "0." ^ String.make (-n) '0' ^ d
    else
      let e = n - 1 in
      let es = (if e >= 0 then "+" else "-") ^ string_of_int (abs e) in
      if k = 1 then d ^ "e" ^ es else String.sub d 0 1 ^ "." ^ String.sub d 1 (k - 1) ^ "e" ^ es

let to_fixed x digits =
  if Float.is_nan x then "NaN"
  else if Float.abs x >= 1e21 then to_string x
  else
    let neg = x < 0. in
    (* Exact decimal expansion of |x| (both libc and JS's toFixed print the
       exact binary value), then round half up at [digits]. *)
    let s = fixed60 (Float.abs x) in
    let dot = String.index s '.' in
    let ip = String.sub s 0 dot and fp = String.sub s (dot + 1) (String.length s - dot - 1) in
    let all = ip ^ String.sub fp 0 digits in
    let up = fp.[digits] >= '5' in
    let all = if up then incr_digits all else all in
    let il = String.length all - digits in
    let body = if digits = 0 then all else String.sub all 0 il ^ "." ^ String.sub all il digits in
    (if neg then "-" else "") ^ body

let to_base36 n =
  if n <= 0. then "0"
  else begin
    let chars = "0123456789abcdefghijklmnopqrstuvwxyz" in
    let buf = ref [] and n = ref (Float.floor n) in
    while !n > 0. do
      let d = Float.rem !n 36. in
      buf := chars.[int_of_float d] :: !buf;
      n := Float.floor (!n /. 36.)
    done;
    String.init (List.length !buf) (List.nth !buf)
  end

(* Howard Hinnant's civil_from_days. *)
let civil_of_days z =
  let z = z + 719468 in
  let era = (if z >= 0 then z else z - 146096) / 146097 in
  let doe = z - (era * 146097) in
  let yoe = (doe - (doe / 1460) + (doe / 36524) - (doe / 146096)) / 365 in
  let y = yoe + (era * 400) in
  let doy = doe - ((365 * yoe) + (yoe / 4) - (yoe / 100)) in
  let mp = ((5 * doy) + 2) / 153 in
  let d = doy - (((153 * mp) + 2) / 5) + 1 in
  let m = if mp < 10 then mp + 3 else mp - 9 in
  ((if m <= 2 then y + 1 else y), m, d)

let iso_utc ms =
  let ms = Float.floor ms in
  let days = Float.floor (ms /. 86400000.) in
  let rest = int_of_float (ms -. (days *. 86400000.)) in
  let y, m, d = civil_of_days (int_of_float days) in
  let s = rest / 1000 in
  let pad w n =
    let s = string_of_int n in
    if String.length s >= w then s else String.make (w - String.length s) '0' ^ s
  in
  pad 4 y ^ "-" ^ pad 2 m ^ "-" ^ pad 2 d ^ "T" ^ pad 2 (s / 3600) ^ ":" ^ pad 2 (s / 60 mod 60) ^ ":" ^ pad 2 (s mod 60)
  ^ "+00:00"

let signed v d = (if v >= 0. then "+" else {js|−|js}) ^ to_fixed (Float.abs v) d
let capitalize = String.capitalize_ascii
