open! Fp

type t = {
  reps : int;
  total_seconds : float;
  avg_median_kg : float;
  avg_sd_kg : float;
  dev_kg : float;
  dev_pct : float;
}

let of_set (st : History.set) =
  let reps = Array.of_list st.reps in
  let col f = Array.map f reps in
  let dev = Stats.mean (col (fun (r : Rep.t) -> r.dev_kg)) in
  {
    reps = Array.length reps;
    total_seconds = Array.fold_left (fun a (r : Rep.t) -> a +. r.seconds) 0. reps;
    avg_median_kg = Stats.mean (col (fun (r : Rep.t) -> r.median_kg));
    avg_sd_kg = Stats.mean (col (fun (r : Rep.t) -> r.sd_kg));
    dev_kg = dev;
    dev_pct = (if st.target_kg <> 0. then dev /. st.target_kg *. 100. else 0.);
  }

type deviation = On_target | Heavy | Light

let rep_deviation (st : History.set) (r : Rep.t) =
  let tol = st.target_kg *. st.tolerance_pct /. 100. in
  let pct = if st.target_kg <> 0. then r.dev_kg /. st.target_kg *. 100. else 0. in
  (pct, if Float.abs r.dev_kg < tol then On_target else if r.dev_kg > 0. then Heavy else Light)
