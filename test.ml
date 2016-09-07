
let time_last = ref (Sys.time ())
let time_reset () = time_last := Sys.time ()
let timer () = Sys.time () -. !time_last

let stat_new () = ref (max_float, min_float, 0., 0)
let stat_add s t =
  let mi, ma, tot, nb = !s in
  s := (min mi t, max ma t, tot +. t, nb + 1)
let stat_print chan s =
  let mi, ma, tot, nb = !s in
  Printf.fprintf chan "\tmin: %.9fs\n\tmax: %.9fs\n\tmean: %.9fs\n\tnb test: %d\n" mi ma (tot/.float_of_int nb) nb


let test n g g_contracted ?labels comp =
  assert(Transportgraph.G.nb_vertex g = Transportgraph.G.nb_vertex g_contracted);
  Transportgraph.stat g;
  Transportgraph.stat g_contracted;
  let vertices = Transportgraph.G.fold_vertex (fun v l -> v::l) g [] in
  let nb_vertices = Transportgraph.G.nb_vertex g in
  let random_vertex () = List.nth vertices (Random.int nb_vertices) in
  let stat_g = stat_new () in
  let stat_g_contracted = stat_new () in
  for i = 1 to n do
    let v1 = random_vertex () in
    let v2 = random_vertex () in
    let t = Random.int 84600 in
    try
      time_reset ();
      ignore (Dijkstra.dijkstra g t v1 v2);
      stat_add stat_g (timer ());
      time_reset ();
      ignore (Dijkstra.bi_dijkstra g_contracted comp t v1 v2);
      stat_add stat_g_contracted (timer ());
    with
      Not_found -> ()
  done;
  Printf.printf "Dijkstra\n-> normal:\n%a\n-> contracted:\n%a%!" stat_print stat_g stat_print stat_g_contracted;
  let stat_g = stat_new () in
  let stat_g_contracted = stat_new () in
  let stat_g_contracted_dp = stat_new () in
  let stat_g_labels = stat_new () in
  for i = 1 to n do begin
    let v1 = random_vertex () in
    let v2 = random_vertex () in
    try
      time_reset ();
      ignore (Timeprofile.time_profile g v1);
      stat_add stat_g (timer ());
      time_reset ();
      ignore (Timeprofile.bi_time_profile g_contracted (fun a b -> comp a b < 0) v1 v2);
      stat_add stat_g_contracted (timer ());
      time_reset ();
      ignore (Timeprofile.bi_time_profile_dp g_contracted (fun a b -> comp a b < 0) v1 v2);
      stat_add stat_g_contracted_dp (timer ());
      begin match labels with
      | None -> ()
      | Some labels ->
          time_reset ();
          ignore (Timeprofile.bi_time_profile_labels labels v1 v2);
          stat_add stat_g_labels (timer ());
      end
    with
      Not_found -> () end;
    Printf.printf "Time profile\n-> normal:\n%a\n-> contracted:\n%a\n-> contracted dp:\n%a\n" stat_print stat_g stat_print stat_g_contracted stat_print stat_g_contracted_dp;
    begin match labels with
    | None -> ()
    | Some _ ->
        Printf.printf "-> labels:\n%a\n" stat_print stat_g_labels;
    end;
    Printf.printf "%!"
  done

(*let test_profile n trips g g_station =
  let vertices = Transportgraph.G.fold_vertex (fun v l -> v::l) g [] in
  let nb_vertices = Transportgraph.G.nb_vertex g in
  let random_vertex () = List.nth vertices (Random.int nb_vertices) in
  let stat_g = stat_new () in
  let stat_g_station = stat_new () in
  for i = 1 to n do begin
    let v1 = random_vertex () in
    let v2 = random_vertex () in
    try
      time_reset ();
      let prof = Transportgraph.H.find (Timeprofile.time_profile g v1) v2 in
      let tot, diff = Label.count_profile (Trip.expand g) trips prof in
      Printf.printf "\ttot: %d\n\tdiff: %d\n" tot diff;
      stat_add stat_g (timer ());
      time_reset ();
      let prof = Transportgraph.H.find (Timeprofile.time_profile g_station v1) v2 in
      let tot, diff = Label.count_profile (Trip.expand g_station) trips prof in
      Printf.printf "\ttot: %d\n\tdiff: %d\n" tot diff;
      stat_add stat_g_station (timer ());
    with
      Not_found -> () end;
    Printf.printf "Time profile\n-> normal:\n%a\n-> station:\n%a%!" stat_print stat_g stat_print stat_g_station
  done*)
