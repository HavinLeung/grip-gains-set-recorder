open! Fp

type rect = { x : float; y : float; w : float; h : float }
type tick = { pos : float; label : string }

type t = {
  width : float;
  height : float;
  plot : rect;
  x0 : float;
  x1 : float;
  y0 : float;
  y1 : float;
  grips : rect list;
  y_ticks : tick list;
  x_labels : tick list;
  band : rect;
  target_y : float;
  line : (float * float) list;
  end_dot : (float * float) option;
  empty : bool;
}

let minus = {js|−|js}

let nice_step range =
  let raw = range /. 4. in
  let mag = Float.pow 10. (Float.floor (Float.log10 raw)) in
  match List.find_opt (fun m -> m *. mag >= raw) [ 1.; 2.; 2.5; 5.; 10. ] with
  | Some m -> m *. mag
  | None -> 10. *. mag

let y_domain s fmin fmax =
  let lower = if Float.is_finite fmin then fmin else 0. and upper = if Float.is_finite fmax then fmax else 10. in
  let td = Settings.to_display s s.target
  and tl = Settings.to_display s (Settings.tolerance_kg s)
  and min_range = Settings.to_display s 2. in
  let lower = Float.min lower (td -. tl) and upper = Float.max upper (td +. tl) in
  let half = Float.max (Float.max (td -. lower) (upper -. td)) (min_range /. 2.) in
  let lower = Float.max 0. (td -. half) and upper = td +. half in
  let upper = if upper <= lower then lower +. min_range else upper in
  (lower, upper)

let x_label_step = function 1 -> 0.25 | 5 -> 1. | 10 -> 2. | _ -> 5.

let layout s b ~x1 ~width ~height =
  let l = 34. and r = 8. and tp = 8. and bt = 20. in
  let pw = width -. l -. r and ph = height -. tp -. bt in
  let n = Force_buffer.length b in
  let ws = float_of_int s.Settings.window_s in
  let x0 = x1 -. ws in
  let i0 = max 0 (Force_buffer.lower_bound b x0 - 1) and i_end = Force_buffer.lower_bound b x1 in
  let disp = Settings.to_display s in
  let fmin = ref Float.infinity and fmax = ref Float.neg_infinity in
  for i = i0 to i_end - 1 do
    let v = disp (Force_buffer.force b i) in
    if v < !fmin then fmin := v;
    if v > !fmax then fmax := v
  done;
  let end_val = if n > 0 then Force_buffer.value_at b x1 else 0. in
  if n > 0 then begin
    let v = disp end_val in
    fmin := Float.min !fmin v;
    fmax := Float.max !fmax v
  end;
  let y0, y1 = y_domain s !fmin !fmax in
  let xp t = l +. ((t -. x0) /. (x1 -. x0) *. pw) in
  let yp v = tp +. ph -. ((v -. y0) /. (y1 -. y0) *. ph) in
  let grips =
    List.filter_map
      (fun (g : Force_buffer.grip) ->
        let e = Option.value ~default:x1 g.stop in
        if e < x0 || g.start > x1 then None
        else
          let a = Float.max l (xp g.start) and bb = Float.min (l +. pw) (xp e) in
          if bb > a then Some { x = a; y = tp; w = bb -. a; h = ph } else None)
      (Force_buffer.grips b)
  in
  let step = nice_step (y1 -. y0) in
  let y_ticks =
    let acc = ref [] and v = ref (Float.ceil (y0 /. step) *. step) in
    while !v <= y1 +. 1e-9 do
      let label = if step < 1. then Jsnum.to_fixed !v 1 else Jsnum.to_string (Jsnum.round !v) in
      acc := { pos = Jsnum.round (yp !v) +. 0.5; label } :: !acc;
      v := !v +. step
    done;
    List.rev !acc
  in
  let x_labels =
    let xs = x_label_step s.window_s in
    let acc = ref [] and sec = ref 0. in
    while !sec <= ws +. 1e-9 do
      let label =
        if !sec = 0. then "now" else minus ^ Jsnum.to_string (float_of_string (Jsnum.to_fixed !sec 2)) ^ "s"
      in
      acc := { pos = l +. pw -. (!sec /. ws *. pw); label } :: !acc;
      sec := !sec +. xs
    done;
    List.rev !acc
  in
  let td = disp s.target and tl = disp (Settings.tolerance_kg s) in
  let band = { x = l; y = yp (td +. tl); w = pw; h = yp (td -. tl) -. yp (td +. tl) } in
  let line, end_dot =
    if n = 0 then ([], None)
    else begin
      let pts = ref [] in
      let pt i = pts := (xp (Force_buffer.time b i), yp (disp (Force_buffer.force b i))) :: !pts in
      let count = i_end - i0 and cols = max 1 (int_of_float (Float.floor (pw *. 1.5))) in
      if count <= cols * 2 then
        for i = i0 to i_end - 1 do
          pt i
        done
      else begin
        let per = float_of_int count /. float_of_int cols in
        for c = 0 to cols - 1 do
          let a = i0 + int_of_float (Float.floor (float_of_int c *. per))
          and bb = min i_end (i0 + int_of_float (Float.floor (float_of_int (c + 1) *. per))) in
          let mn = ref Float.infinity and mx = ref Float.neg_infinity and mi = ref a and xi = ref a in
          for i = a to bb - 1 do
            let v = Force_buffer.force b i in
            if v < !mn then begin
              mn := v;
              mi := i
            end;
            if v > !mx then begin
              mx := v;
              xi := i
            end
          done;
          pt (min !mi !xi);
          pt (max !mi !xi)
        done
      end;
      let ey = yp (disp end_val) in
      pts := (xp x1, ey) :: !pts;
      (List.rev !pts, Some (l +. pw -. 1., ey))
    end
  in
  {
    width;
    height;
    plot = { x = l; y = tp; w = pw; h = ph };
    x0;
    x1;
    y0;
    y1;
    grips;
    y_ticks;
    x_labels;
    band;
    target_y = yp td;
    line;
    end_dot;
    empty = n = 0;
  }

let render_time ~streaming ~now ~render_delay_ms ~frozen ~last_sample =
  if streaming then
    let t = (now -. render_delay_ms) /. 1000. in
    (t, Some t)
  else
    match (frozen, last_sample) with
    | Some f, _ -> (f, frozen)
    | None, Some t -> (t, None)
    | None, None -> (now /. 1000., None)

type rep_graph = {
  rep_width : float;
  rep_height : float;
  full : (float * float) list;
  stable_line : (float * float) list;
  rep_target_y : float;
  left : float;
  right : float;
  top : float;
  bottom : float;
  axis : (string * string * string) option;
  rep_min : float;
  rep_max : float;
}

let rep_graph s (rep : Rep.t) ~target_kg ~width ~height ~axis =
  let f = rep.force_kg in
  let len = Array.length f in
  if len < 2 then None
  else begin
    let l = if axis then 30. else 1. and bt = if axis then 14. else 2. and tp = 2. in
    let pw = width -. l -. 1. and ph = height -. tp -. bt in
    let mn = ref Float.infinity and mx = ref Float.neg_infinity in
    Array.iter
      (fun v ->
        if v < !mn then mn := v;
        if v > !mx then mx := v)
      f;
    let mn = Float.max 0. (Float.min !mn target_kg) and mx = Float.max !mx target_kg in
    let mx = if mx -. mn < 0.5 then mn +. 0.5 else mx in
    let xp i = l +. (float_of_int i /. float_of_int (len - 1) *. pw) in
    let yp v = tp +. ph -. ((v -. mn) /. (mx -. mn) *. ph) in
    let seg a b = List.init (b - a + 1) (fun k -> (xp (a + k), yp f.(a + k))) in
    let a, b = rep.stable in
    let round_label v = Jsnum.to_string (Jsnum.round (Settings.to_display s v)) in
    Some
      {
        rep_width = width;
        rep_height = height;
        full = seg 0 (len - 1);
        stable_line = (if b > a then seg (min a (len - 1)) (min b (len - 1)) else []);
        rep_target_y = yp target_kg;
        left = l;
        right = l +. pw;
        top = tp;
        bottom = tp +. ph;
        axis = (if axis then Some (round_label mx, round_label mn, Jsnum.to_fixed rep.seconds 1 ^ "s") else None);
        rep_min = mn;
        rep_max = mx;
      }
  end
