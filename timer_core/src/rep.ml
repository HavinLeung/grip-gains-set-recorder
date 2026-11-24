open! Fp

type percentiles = {
  p1 : float;
  p5 : float;
  p10 : float;
  q1 : float;
  med : float;
  q3 : float;
  p90 : float;
  p95 : float;
  p99 : float;
}

type analysis = { stable : int * int; median : float; mean : float; sd : float; pct : percentiles; dev_kg : float }

let r2 = Jsnum.r2

let analyze samples ~target_kg =
  let n = Array.length samples in
  let start = ref 0 and stop = ref (max 0 (n - 1)) in
  if n >= 4 then begin
    let q = n asr 2 in
    let mid = Array.sub samples q (n - (2 * q)) in
    let bm = Stats.median mid and bs = Stats.pop_sd mid in
    let lo = bm -. (3. *. bs) and hi = bm +. (3. *. bs) in
    let inside x = x >= lo && x <= hi in
    (try
       for i = 0 to n - 1 do
         if inside samples.(i) then begin
           start := i;
           raise Exit
         end
       done
     with Exit -> ());
    try
      for i = n - 1 downto !start do
        if inside samples.(i) then begin
          stop := i;
          raise Exit
        end
      done
    with Exit -> ()
  end;
  let f = if !start <= !stop then Array.sub samples !start (min n (!stop + 1) - !start) else samples in
  let sorted = Stats.sorted f in
  let med = Stats.median f in
  let p x = r2 (Stats.percentile_sorted sorted x) in
  {
    stable = (!start, !stop);
    median = r2 med;
    mean = r2 (Stats.mean f);
    sd = r2 (Stats.sd f *. 1000.) /. 1000.;
    pct =
      {
        p1 = p 0.01;
        p5 = p 0.05;
        p10 = p 0.10;
        q1 = p 0.25;
        med = r2 med;
        q3 = p 0.75;
        p90 = p 0.90;
        p95 = p 0.95;
        p99 = p 0.99;
      };
    dev_kg = r2 (med -. target_kg);
  }

let store_every = 4

type t = {
  seconds : float;
  median_kg : float;
  mean_kg : float;
  sd_kg : float;
  dev_kg : float;
  pct_kg : percentiles;
  stable : int * int;
  graph_hz : float;
  force_kg : float array;
  extra : (string * Json.t) list;
}

let make ~duration samples ~target_kg =
  let a = analyze samples ~target_kg in
  let n = Array.length samples in
  let force =
    Array.init ((n + store_every - 1) / store_every) (fun k ->
        let i = k * store_every in
        r2 (Stats.mean (Array.sub samples i (min store_every (n - i)))))
  in
  let s0, s1 = a.stable in
  {
    seconds = r2 duration;
    median_kg = a.median;
    mean_kg = a.mean;
    sd_kg = a.sd;
    dev_kg = a.dev_kg;
    pct_kg = a.pct;
    stable = (s0 / store_every, s1 / store_every);
    graph_hz = 80. /. float_of_int store_every;
    force_kg = force;
    extra = [];
  }

let percentile_list p =
  [
    ("P1", p.p1);
    ("P5", p.p5);
    ("P10", p.p10);
    ("Q1", p.q1);
    ("Med", p.med);
    ("Q3", p.q3);
    ("P90", p.p90);
    ("P95", p.p95);
    ("P99", p.p99);
  ]

let num f = Json.Number f

let to_json r =
  let a, b = r.stable in
  Json.Object
    ([
       ("seconds", num r.seconds);
       ("median_kg", num r.median_kg);
       ("mean_kg", num r.mean_kg);
       ("sd_kg", num r.sd_kg);
       ("dev_kg", num r.dev_kg);
       ("pct_kg", Json.Object (List.map (fun (k, v) -> (k, num v)) (percentile_list r.pct_kg)));
       ("stable", Json.Array [ num (float_of_int a); num (float_of_int b) ]);
       ("graph_hz", num r.graph_hz);
       ("force_kg", Json.Array (Array.to_list (Array.map num r.force_kg)));
     ]
    @ r.extra)

let known = [ "seconds"; "median_kg"; "mean_kg"; "sd_kg"; "dev_kg"; "pct_kg"; "stable"; "graph_hz"; "force_kg" ]

let of_json j =
  match j with
  | Json.Object fields -> (
      let numd ?(default = 0.) k o = Option.value ~default (Option.bind (Json.member k o) Json.to_number) in
      match Option.bind (Json.member "seconds" j) Json.to_number with
      | None -> Error "rep without seconds"
      | Some seconds ->
          let pct = Option.value ~default:(Json.Object []) (Json.member "pct_kg" j) in
          let p k = numd k pct in
          let ints =
            match Option.bind (Json.member "stable" j) Json.to_list with
            | Some [ Json.Number a; Json.Number b ] -> (int_of_float a, int_of_float b)
            | _ -> (0, 0)
          in
          let force =
            match Option.bind (Json.member "force_kg" j) Json.to_list with
            | Some l -> Array.of_list (List.map (fun v -> Option.value ~default:0. (Json.to_number v)) l)
            | None -> [||]
          in
          Ok
            {
              seconds;
              median_kg = numd "median_kg" j;
              mean_kg = numd "mean_kg" j;
              sd_kg = numd "sd_kg" j;
              dev_kg = numd "dev_kg" j;
              pct_kg =
                {
                  p1 = p "P1";
                  p5 = p "P5";
                  p10 = p "P10";
                  q1 = p "Q1";
                  med = p "Med";
                  q3 = p "Q3";
                  p90 = p "P90";
                  p95 = p "P95";
                  p99 = p "P99";
                };
              stable = ints;
              graph_hz = numd ~default:20. "graph_hz" j;
              force_kg = force;
              extra = List.filter (fun (k, _) -> not (List.mem k known)) fields;
            })
  | _ -> Error "rep is not an object"
