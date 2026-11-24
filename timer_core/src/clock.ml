open! Fp

type anchor = { device : float; page : float }
type t = {
  anchor : anchor option;
  last_page : float option;
  last_device : float;
  device_s : float;
  offsets : float list;
}

type time = { page_s : float; device_s : float }

let empty = { anchor = None; last_page = None; last_device = 0.; device_s = 0.; offsets = [] }
let reset_anchor c = { c with anchor = None; offsets = [] }
let offset_window = 25

let device_at c ~page_s =
  match c.offsets with [] -> None | o :: rest -> Some (page_s -. List.fold_left Float.min o rest)
let forget_last c = { c with last_page = None }
let min_step = 0.0125
let max_lead = 10.
let max_step_us = 1e6
let half_range = 2147483648.
let after_last c = match c.last_page with Some l -> l +. min_step | None -> Float.neg_infinity

(* Seconds from sample [i] to the batch's newest: device time within each run
   of consecutive samples, [min_step] across a break (a counter restart inside
   the notification). Without a break: [to_uint32 (last − first) / 1e6], the
   MVP's span. *)
let span_to_end (samples : Protocol.sample array) i =
  let n = Array.length samples in
  let span = ref 0. and j = ref i in
  while !j < n - 1 do
    let k = ref !j in
    while !k < n - 1 && Jsnum.to_uint32 (samples.(!k + 1).device_us -. samples.(!k).device_us) <= max_step_us do
      incr k
    done;
    span := !span +. (Jsnum.to_uint32 (samples.(!k).device_us -. samples.(!j).device_us) /. 1e6);
    if !k < n - 1 then span := !span +. min_step;
    j := !k + 1
  done;
  !span

(* The page time of [ts] on the current anchor, if [ts] continues the stream:
   the counter moved forward (modulo 2^32, so a wrap is continuous) and the
   sample does not land more than [max_lead] ahead of now. An anchor more than
   half the counter's range old is first moved to the previous sample (same
   mapping), so the modular difference stays unambiguous in long sessions. *)
let continue_anchor c ~now_s ts =
  match c.anchor with
  | Some a when Jsnum.to_uint32 (ts -. c.last_device) < half_range ->
      let a =
        if Jsnum.to_uint32 (ts -. a.device) >= half_range then
          { device = c.last_device; page = a.page +. (Jsnum.to_uint32 (c.last_device -. a.device) /. 1e6) }
        else a
      in
      let disp = a.page +. (Jsnum.to_uint32 (ts -. a.device) /. 1e6) in
      if disp <= now_s +. max_lead then Some (a, disp) else None
  | _ -> None

let end_batch c ~now_s =
  match (c.last_page, c.anchor) with
  | Some last, Some a ->
      let lead = last -. now_s in
      let page = if lead < -0.6 then a.page -. lead else a.page -. (lead *. 0.05) in
      { c with anchor = Some { a with page } }
  | _ -> c

let map_batch c ~now_s (samples : Protocol.sample array) =
  let c = ref c and reanchored = ref false in
  let times =
    Array.mapi
      (fun i (s : Protocol.sample) ->
        let ts = s.device_us in
        let anchor, disp, device_s =
          match continue_anchor !c ~now_s ts with
          | Some (a, disp) -> (a, disp, !c.device_s +. (Jsnum.to_uint32 (ts -. !c.last_device) /. 1e6))
          | None ->
              let page = Float.max (now_s -. span_to_end samples i) (after_last !c) in
              let gap = match !c.last_page with Some l -> page -. l | None -> 0. in
              reanchored := true;
              ({ device = ts; page }, page, !c.device_s +. gap)
        in
        c := { !c with anchor = Some anchor; last_page = Some disp; last_device = ts; device_s };
        { page_s = disp; device_s })
      samples
  in
  (* Radio delays only ever make a notification late: the earliest one of the
     last few seconds gives the best offset between the two clocks. Device
     time across a new anchor is only an estimate, so the window restarts. *)
  if Array.length samples > 0 then begin
    let recent = if !reanchored then [] else List.filteri (fun i _ -> i < offset_window - 1) !c.offsets in
    c := { !c with offsets = (now_s -. !c.device_s) :: recent }
  end;
  (end_batch !c ~now_s, times)
