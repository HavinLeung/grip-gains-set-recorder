(* Canvas drawing for the live graph and the saved rep graphs (the MVP's drawGraph /
   drawRepGraph, lines 1234-1359). Timer_core.Graph computes every coordinate and label; this
   file only strokes and fills, with the MVP's colours (CSS variables, read at most once a second
   so a light/dark switch is picked up), fonts and device-pixel-ratio handling. *)

open Timer_core

type colors = {
  mutable ink : string;
  mutable muted : string;
  mutable line : string;
  mutable accent : string;
  mutable ok : string;
  mutable band : string;
  mutable grip : string;
  mutable read_at : float;
}

let colors =
  {
    ink = "#141A23";
    muted = "#5D6878";
    line = "#DCE1E8";
    accent = "#2D5BE3";
    ok = "#18864B";
    band = "";
    grip = "";
    read_at = neg_infinity;
  }

let read_colors el =
  let v n = Browser.css_var el ("--" ^ n) in
  colors.ink <- v "ink";
  colors.muted <- v "muted";
  colors.line <- v "line";
  colors.accent <- v "accent";
  colors.ok <- v "ok";
  colors.band <- v "band";
  colors.grip <- v "grip";
  colors.read_at <- Browser.now ()

let maybe_read_colors el =
  if Browser.now () -. colors.read_at > 1000. then read_colors el

(* Size the backing store to CSS size × DPR and draw in CSS pixels. *)
let fit cv c =
  let dpr = Browser.dpr () in
  let w = Browser.canvas_client_width cv
  and h = Browser.canvas_client_height cv in
  let bw = int_of_float (Jsnum.round (w *. dpr))
  and bh = int_of_float (Jsnum.round (h *. dpr)) in
  if Browser.canvas_width cv <> bw || Browser.canvas_height cv <> bh then begin
    Browser.set_canvas_width cv bw;
    Browser.set_canvas_height cv bh
  end;
  Browser.set_transform c dpr 0. 0. dpr 0. 0.;
  (w, h)

let polyline c pts =
  match pts with
  | [] -> ()
  | (x, y) :: rest ->
      Browser.move_to c x y;
      List.iter (fun (x, y) -> Browser.line_to c x y) rest

let live cv (s : Settings.t) buffer ~x1 =
  let c = Browser.get_context cv "2d" in
  let w, h = fit cv c in
  Browser.clear_rect c 0. 0. w h;
  if w > 0. && h > 0. then begin
    let g = Graph.layout s buffer ~x1 ~width:w ~height:h in
    let p = g.plot in
    (* rep shading *)
    Browser.set_fill_style c colors.grip;
    List.iter
      (fun (r : Graph.rect) -> Browser.fill_rect c r.x r.y r.w r.h)
      g.grips;
    Browser.set_font c "500 11px Inter, system-ui, sans-serif";
    Browser.set_text_baseline c "middle";
    Browser.set_text_align c "right";
    Browser.set_stroke_style c colors.line;
    Browser.set_line_width c 1.;
    Browser.set_fill_style c colors.muted;
    List.iter
      (fun (tk : Graph.tick) ->
        Browser.begin_path c;
        Browser.move_to c p.x tk.pos;
        Browser.line_to c (p.x +. p.w) tk.pos;
        Browser.stroke c;
        Browser.fill_text c tk.label (p.x -. 6.) tk.pos)
      g.y_ticks;
    Browser.set_text_align c "center";
    Browser.set_text_baseline c "alphabetic";
    List.iter
      (fun (tk : Graph.tick) -> Browser.fill_text c tk.label tk.pos (h -. 5.))
      g.x_labels;
    Browser.set_fill_style c colors.band;
    Browser.fill_rect c g.band.x g.band.y g.band.w g.band.h;
    Browser.set_stroke_style c colors.ok;
    Browser.set_line_dash c [| 5.; 3. |];
    Browser.set_line_width c 1.2;
    Browser.begin_path c;
    Browser.move_to c p.x g.target_y;
    Browser.line_to c (p.x +. p.w) g.target_y;
    Browser.stroke c;
    Browser.set_line_dash c [||];
    Browser.save c;
    Browser.begin_path c;
    Browser.rect c p.x p.y p.w p.h;
    Browser.clip c;
    if g.line <> [] then begin
      Browser.set_stroke_style c colors.accent;
      Browser.set_line_width c 2.;
      Browser.set_line_join c "round";
      Browser.begin_path c;
      polyline c g.line;
      Browser.stroke c;
      match g.end_dot with
      | Some (x, y) ->
          Browser.set_fill_style c colors.accent;
          Browser.begin_path c;
          Browser.arc c x y 3.5 0. (2. *. Float.pi);
          Browser.fill c
      | None -> ()
    end;
    Browser.restore c;
    if g.empty then begin
      Browser.set_fill_style c colors.muted;
      Browser.set_text_align c "center";
      Browser.set_font c "500 13px Inter, system-ui, sans-serif";
      Browser.fill_text c "Force will draw here once readings arrive"
        (p.x +. (p.w /. 2.))
        (p.y +. (p.h /. 2.))
    end
  end

(* Saved force (20 Hz), with the stable section the stats use in the accent colour and the target
   dashed. *)
let rep cv (s : Settings.t) (r : Rep.t) ~target_kg ~axis =
  let c = Browser.get_context cv "2d" in
  let w, h = fit cv c in
  Browser.clear_rect c 0. 0. w h;
  match Graph.rep_graph s r ~target_kg ~width:w ~height:h ~axis with
  | None -> ()
  | Some g ->
      (match g.axis with
      | Some (mx, mn, dur) ->
          Browser.set_font c "500 10px Inter, system-ui, sans-serif";
          Browser.set_fill_style c colors.muted;
          Browser.set_text_align c "right";
          Browser.set_text_baseline c "middle";
          Browser.fill_text c mx (g.left -. 4.) (g.top +. 5.);
          Browser.fill_text c mn (g.left -. 4.) (g.bottom -. 2.);
          Browser.set_text_align c "left";
          Browser.set_text_baseline c "alphabetic";
          Browser.fill_text c "0s" g.left (h -. 2.);
          Browser.set_text_align c "right";
          Browser.fill_text c dur g.right (h -. 2.)
      | None -> ());
      Browser.set_stroke_style c colors.ok;
      Browser.set_line_dash c [| 3.; 2. |];
      Browser.set_line_width c 1.;
      Browser.begin_path c;
      Browser.move_to c g.left g.rep_target_y;
      Browser.line_to c g.right g.rep_target_y;
      Browser.stroke c;
      Browser.set_line_dash c [||];
      let seg pts col =
        Browser.set_stroke_style c col;
        Browser.set_line_width c 1.5;
        Browser.begin_path c;
        polyline c pts;
        Browser.stroke c
      in
      seg g.full colors.line;
      if g.stable_line <> [] then seg g.stable_line colors.accent
