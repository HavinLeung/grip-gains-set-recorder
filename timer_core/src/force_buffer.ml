open! Fp

type grip = { start : float; mutable stop : float option }

type t = {
  mutable ts : float array;
  mutable fs : float array;
  mutable first : int;
  mutable next : int;
  mutable newest_grips : grip list;  (** Newest first. *)
}

let max_history_s = 600.
let create () = { ts = Array.make 1024 0.; fs = Array.make 1024 0.; first = 0; next = 0; newest_grips = [] }

let clear b =
  b.first <- 0;
  b.next <- 0;
  b.newest_grips <- []

let length b = b.next - b.first
let time b i = b.ts.(b.first + i)
let force b i = b.fs.(b.first + i)
let last_time b = if length b = 0 then None else Some b.ts.(b.next - 1)
let grips b = List.rev b.newest_grips

let make_room b =
  let n = length b and cap = Array.length b.ts in
  let cap' = if n * 2 > cap then cap * 2 else cap in
  let ts = if cap' = cap then b.ts else Array.make cap' 0. in
  let fs = if cap' = cap then b.fs else Array.make cap' 0. in
  Array.blit b.ts b.first ts 0 n;
  Array.blit b.fs b.first fs 0 n;
  b.ts <- ts;
  b.fs <- fs;
  b.first <- 0;
  b.next <- n

let push b t f =
  if b.next = Array.length b.ts then make_room b;
  b.ts.(b.next) <- t;
  b.fs.(b.next) <- f;
  b.next <- b.next + 1;
  if length b mod 400 = 0 && t -. b.ts.(b.first) > max_history_s +. 5. then begin
    let i = ref b.first in
    while !i < b.next && b.ts.(!i) < t -. max_history_s do
      incr i
    done;
    b.first <- !i;
    let t0 = b.ts.(b.first) in
    b.newest_grips <- List.filter (fun g -> match g.stop with None -> true | Some e -> e >= t0) b.newest_grips
  end

let start_grip b s = b.newest_grips <- { start = s; stop = None } :: b.newest_grips

let end_grip b e =
  match b.newest_grips with g :: _ when g.stop = None -> g.stop <- Some e | _ -> ()

let discard_grip b = match b.newest_grips with g :: rest when g.stop = None -> b.newest_grips <- rest | _ -> ()

let apply b (e : Session.event) =
  match e with
  | Sample { t; force } -> push b t force
  | Buffer_cleared -> clear b
  | Grip_started s -> start_grip b s
  | Grip_ended e -> end_grip b e
  | Grip_discarded -> discard_grip b
  | _ -> ()

let lower_bound b x =
  let lo = ref 0 and hi = ref (length b) in
  while !lo < !hi do
    let m = (!lo + !hi) asr 1 in
    if time b m < x then lo := m + 1 else hi := m
  done;
  !lo

let value_at b t =
  let n = length b in
  if n = 0 then 0.
  else
    let i = lower_bound b t in
    if i >= n then force b (n - 1)
    else if i = 0 then force b 0
    else
      let t0 = time b (i - 1) and t1 = time b i in
      if t1 > t0 then force b (i - 1) +. ((force b i -. force b (i - 1)) *. ((t -. t0) /. (t1 -. t0))) else force b i
