(** The live force graph's geometry (the MVP's [drawGraph]): everything it
    computes, as draw-ready values in CSS pixels, so the UI only strokes and
    fills. Colours, fonts and the device-pixel-ratio transform stay in the
    UI.

    Layout: plot area inset 34 px left, 8 right, 8 top, 20 bottom. The window
    is [[x1 − window_s, x1]] page seconds, where [x1] is the render time
    ({!render_time}). Forces are drawn in display units; the y-domain is
    centred on the target and always shows the tolerance band. When a window
    holds more than [2 · cols] samples ([cols = floor(1.5 · plot width)]),
    each column keeps only its min and max (in time order) so peaks
    survive. *)

type rect = { x : float; y : float; w : float; h : float }
type tick = { pos : float;  (** Pixel y (gridlines) or x (time labels). *) label : string }

type t = {
  width : float;
  height : float;
  plot : rect;
  x0 : float;  (** Window start, page s. *)
  x1 : float;
  y0 : float;  (** Domain, display units. *)
  y1 : float;
  grips : rect list;  (** Rep shading, clipped to the plot, in grip order. *)
  y_ticks : tick list;
      (** Gridlines at [round(Y(v)) + 0.5], from the bottom up; labels to the
          left of the plot, right-aligned at [plot.x − 6]. *)
  x_labels : tick list;  (** ["now"], ["−1s"], …, centred at [pos], baseline [height − 5]. *)
  band : rect;  (** Tolerance band, full plot width. *)
  target_y : float;  (** Dashed target line. *)
  line : (float * float) list;
      (** The force polyline (clip to [plot]); ends at [(right edge, end_y)].
          Empty when there are no samples. *)
  end_dot : (float * float) option;  (** 3.5 px dot at the right edge. *)
  empty : bool;  (** No samples: show "Force will draw here once readings arrive" centred. *)
}

val layout : Settings.t -> Force_buffer.t -> x1:float -> width:float -> height:float -> t

val nice_step : float -> float
(** Gridline spacing for a range: the smallest of 1, 2, 2.5, 5, 10 × 10ᵏ
    that is at least a quarter of it. *)

val y_domain : Settings.t -> float -> float -> float * float
(** [yDomain(fmin, fmax)] in display units ([ForceGraphView.yAxisDomain]):
    symmetric about the target, at least the tolerance band and 2 kg tall,
    clamped at 0. Non-finite bounds read as 0 / 10. *)

val x_label_step : int -> float
(** Seconds between time labels: 0.25, 1, 2, 5 for 1, 5, 10, 30 s windows. *)

val render_time :
  streaming:bool -> now:float -> render_delay_ms:float -> frozen:float option -> last_sample:float option ->
  float * float option
(** The frame loop's render time and the new frozen time: while streaming,
    [(now − delay) / 1000] (and remember it); otherwise the frozen time, else
    the last sample's time, else [now / 1000]. *)

(** {2 Saved rep graphs} ([drawRepGraph]) *)

type rep_graph = {
  rep_width : float;
  rep_height : float;
  full : (float * float) list;  (** Whole rep, muted colour. *)
  stable_line : (float * float) list;  (** The stable section (accent); empty if none. *)
  rep_target_y : float;  (** Dashed target line from [left] to [right]. *)
  left : float;
  right : float;
  top : float;
  bottom : float;
  axis : (string * string * string) option;
      (** With axes: max and min labels (display units, rounded; right-aligned
          at [left − 4], at [y = top + 5] and [bottom − 2]) and the duration
          (e.g. ["7.3s"], right-aligned at [right], baseline [height − 2]);
          ["0s"] goes at [left], same baseline. *)
  rep_min : float;  (** Domain, kg. *)
  rep_max : float;
}

val rep_graph : Settings.t -> Rep.t -> target_kg:float -> width:float -> height:float -> axis:bool -> rep_graph option
(** [None] when the rep has fewer than two stored points (nothing drawn). *)
