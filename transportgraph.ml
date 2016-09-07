open Utils
open Gtfs

module Vertex =
  struct
    type t = stop_id
    let compare = compare
    let hash = Hashtbl.hash
    let equal = (=)
  end

module Edge =
  struct
    type t = Label.t ref
    let compare = compare
    let equal = (=)
    let default = ref Label.empty
  end

module G = Graph.Imperative.Digraph.ConcreteBidirectionalLabeled(Vertex)(Edge)
(* accessing predecessors and successors is in O(1) amortized *)

module H = Hashtbl.Make(G.V)

type t = G.t


(** Merge to label or create a new edge if it does not exist *)
let add_label g src dst label =
  let c, w = Label.ty label in
  if c || w then (* Label not empty *)
    try
      let edge = G.find_edge g src dst in
      let new_label = Label.merge !(G.E.label edge) label in
      G.E.label edge := new_label
    with Not_found -> G.add_edge_e g (G.E.create src (ref label) dst)


(** create a graph from GTFS *)
let load_aux ~dir ~date =
  let connections = Gtfs.connections ~dir:dir ~date:date in
  let transfers = Gtfs.transfers dir in
  let g = G.create () in
  let add_edge_conn (dep_p, arr_p) conn =
    let label = Label.create conn None dep_p arr_p in
    add_label g dep_p arr_p label
  in
  let add_edge_walk (dep_p, arr_p, t) =
    let label1 = Label.create [] (Some t) dep_p arr_p in
    let label2 = Label.create [] (Some t) arr_p dep_p in
    add_label g dep_p arr_p label1;
    add_label g arr_p dep_p label2
  in
  (* add connections *)
  Hashtbl.iter add_edge_conn connections;
  (* add transfers *)
  List.iter add_edge_walk transfers;
  g


(** Print statistics about g *)
let stat g =
  let open Printf in
  printf "------------\n";
  let nb_v, nb_e = G.nb_vertex g, G.nb_edges g in
  printf "|V| = %d\n|E| = %d\n" nb_v nb_e;
  let i_max, i_tot, o_max, o_tot =
    G.fold_vertex
      (fun v (i_max, i_tot, o_max, o_tot) ->
        let deg_i = G.in_degree g v in
        let deg_o = G.out_degree g v in
        max i_max deg_i, i_tot + deg_i, max o_max deg_o, o_tot + deg_o
      )
      g
      (0, 0, 0, 0)
  in
  printf "Degrees\n\tin  max/mean %d/%d\n\tout max/mean %d/%d\n" i_max (i_tot/nb_v) o_max (o_tot/nb_v);
  let l_max, l_tot, c, w, b =
    G.fold_edges_e
      (fun e (l_max, l_tot, c, w, b) ->
        let lab = !(G.E.label e) in
        let size = Label.size lab in
        let c', w' = Label.ty lab in
        max l_max size, l_tot + size, (if c' then c+1 else c), (if w' then w+1 else w), (if c' && w' then b+1 else b)
      )
      g
      (0, 0, 0, 0, 0)
  in
  printf "Label sizes max/mean %d/%d\n" l_max (try l_tot/c with Division_by_zero -> 0);
  printf "\twith walk %d (%d%%)\n\twith transport %d (%d%%)\n\twith both %d (%d%%)\n" w (100*w/nb_e) c (100*c/nb_e) b (100*b/nb_e);
  printf "------------\n%!"


(** Contracts nodes without connections and remove them *)
let contract_only_walking g =
  let contract pred succ =
    let src = G.E.src pred in
    let dst = G.E.dst succ in
    if src <> dst then begin
      (* we do not use Label.link because it will recall a contraction *)
      let w1 = Label.get_w !(G.E.label pred) in
      let w2 = Label.get_w !(G.E.label succ) in
      add_label g src dst (Label.create [] (Some (w1+w2)) src dst)
    end
  in
  let foreach_vertex v =
    let pure_walk e r =
      let conn, _ = Label.ty !(G.E.label e) in
      r && not conn
    in
    let pure = G.fold_succ_e pure_walk g v (G.fold_pred_e pure_walk g v true) in
    if pure then begin
      G.iter_pred_e (fun pred -> G.iter_succ_e (contract pred) g v) g v;
      G.remove_vertex g v (* TODO remove during iter *)
    end
  in
  G.iter_vertex foreach_vertex g


let load ~dir ~date =
  let g = load_aux ~dir:dir ~date:date in
  contract_only_walking g;
  (* Change le graphe pour n'avoir que les temps de parcours minimum de chaque arete *)
  (*let g' = G.create ~size:(G.nb_vertex g) () in
  G.iter_vertex (G.add_vertex g') g;
  G.iter_edges_e
    (fun (s,l,d) ->
      let e' = (s, Label.create [] (Some (Label.mint l)) s d, d) in
      G.add_edge_e g' e'
    )
    g;
  g'*)
  (*G.iter_edges (fun v v' -> assert(List.length (G.find_all_edges g v v') = 1)) g;*)
  (* remove loops *)
  G.iter_vertex (fun v -> G.remove_edge g v v) g;
  g


module VUF = Unionfind.Make(G.V)

let components_walk g =
  let all_vertices = (G.fold_vertex (fun v l -> v::l) g []) in
  let uf = VUF.init all_vertices in
  G.iter_edges_e (fun e ->
    let _, w = Label.ty !(G.E.label e) in
    if w then
      VUF.union (G.E.src e) (G.E.dst e) uf
    ) g;
  all_vertices, uf

let contract_stations g =
  let all_vertices, uf = components_walk g in
  List.iter
    (fun v ->
      let v' = VUF.find v uf in
      if v <> v' then begin
        (* contract v and v' *)
        G.iter_pred_e (fun e -> if G.E.src e <> v' then add_label g (G.E.src e) v' !(G.E.label e)) g v;
        G.iter_succ_e (fun e -> if G.E.dst e <> v' then add_label g v' (G.E.dst e) !(G.E.label e)) g v;
        G.remove_vertex g v
      end)
    all_vertices;
  fun v -> VUF.find v uf


let station_graph g stops =
  let all_vertices, uf = components_walk g in
  let centers = H.create 97 in (* centers of stars *)
  let stops_centers = H.create 97 in
  (* create centers *)
  List.iter
    (fun v ->
      let v' = VUF.find v uf in
      let size = VUF.size v uf in
      if size > 2 && v = v' then begin
        let center = stop_id_fresh () in
        G.add_vertex g center;
        H.add centers v center;
        let name, lat, lng = stops v in
        H.add stops_centers center ("center_" ^ name, lat, lng) (* the center is put at the same place *)
      end
    )
    all_vertices;
  (* remove walk edges *)
  G.iter_edges_e
    (fun e ->
      if VUF.size (G.E.src e) uf > 2 then begin
        let conn, walk = Label.ty !(G.E.label e) in
        if not conn && walk then
          G.remove_edge_e g e (* TODO remove pendant iter ? *)
        else if walk then
          () (* TODO enlever la partie walk du label *)
      end
    ) g;
  (* add walk to centers *)
  List.iter
    (fun v ->
      let v', size = VUF.find v uf, VUF.size v uf in
      if size > 2 then begin
        let center = H.find centers v' in
        let label = Label.create [] (Some 150) v center in
        add_label g v center label;
        let label = Label.create [] (Some 150) center v in
        add_label g center v label
      end
    )
    all_vertices;
  fun v ->
    try
      stops v
    with
      Not_found -> H.find stops_centers v

(* equirectangular projection *)
let project lat0 (lat, lng) =
  let r = 6400000. in (* Earth radius *)
  let pi = 3.1415 in
  let rad_of_deg x = x *. pi /. 180. in
  r *. (rad_of_deg lng) *. cos (rad_of_deg lat0),
  r *. (rad_of_deg lat)

let add_dimacs g gr co stops trips =
  (* range !!! magic numbers *)
  let lng_min, lng_max = 2., 2.80 in
  let lat_min, lat_max = 48.68, 49.06 in
  let lat0 = (lat_min +. lat_max) /. 2. in
  let project = project lat0 in
  let vertices = Hashtbl.create 97 in (* dimacs v -> stop_id *)
  let stops_new = H.create 97 in (* stop_id -> name * lat * lng *)
  (* add vertices *)
  let chan = open_in co in
  let ignored = ref 0 in
  Dimacs9.parse_co
    (fun u lng lat ->
      if lng >= lng_min && lng < lng_max && lat >= lat_min && lat < lat_max then begin
        let name = Printf.sprintf "dimacs_%d" u in
        let stop_id = stop_id_of_str name in
        H.add stops_new stop_id (name, lat, lng);
        Hashtbl.add vertices u stop_id;
        G.add_vertex g stop_id
      end
      else
        incr ignored
    ) chan;
  close_in chan;
  if !ignored > 0 then
    Printf.printf "%d outranged vertices ignored\n%!" !ignored;
  (* add edges *)
  let chan = open_in gr in
  let not_found = ref 0 in
  Dimacs9.parse_gr
    (fun u v w ->
      try
        let u = Hashtbl.find vertices u in
        let v = Hashtbl.find vertices v in
        let t = w * 6 / 100 in (* 6km/h, w in decimeters *)
        add_label g u v (Label.create [] (Some t) u v);
        add_label g v u (Label.create [] (Some t) v u)
      with Not_found -> incr not_found
    ) chan;
  if !not_found > 0 then
    Printf.eprintf "WARNING %d edges ignored because one or more endpoints are unknown\n%!" !not_found;
  close_in chan;
  (* TODO add a walk between each bus stop and the nearest vertex (projection sur le segment ?) *)
  (* we have to check trips that stops to a stop_id to known if it is a bus stop or not *)
  let is_in_surface label =
    IFDEF TRIP THEN
    let rep = ref false in
    Label.iter_c
      (fun { info } ->
        match info with
        | Transport trip_id -> if snd (StringMap.find trip_id trips) <> 1 then rep := true (* 1 = underground *)
        | Walk -> ()
        | _ -> failwith "do not call Transportgraph.add_dimacs with a contracted graph"
      )
      label;
    !rep
    ELSE
    failwith "not implemented" (* TODO detecter les stations en surface dès la lecture gtfs *)
    END
  in
  let is_in_surface g v =
    let surf = G.fold_succ_e (fun e surf -> surf || is_in_surface !(G.E.label e)) g v false in
    G.fold_pred_e (fun e surf -> surf || is_in_surface !(G.E.label e)) g v surf
  in
  Printf.printf "Add connections between the two graphs\n%!";
  let box = (* box containing all the range *)
    let lx, ly = project (lat_min, lng_min) in
    let ux, uy = project (lat_max, lng_max) in
    let diameter = max (ux -. lx) (uy -. ly) in
    let cx = (ux +. lx) /. 2. in
    let cy = (uy +. ly) /. 2. in
    (cx, cy), diameter /. 2.
  in
  let qt = Quadtree.create box in
  let qt = H.fold (fun id (_, lat, lng) qt -> Quadtree.insert qt (project (lat, lng), id)) stops_new qt in
  let tot_surface = ref 0 in
  G.iter_vertex
    (fun v ->
      if not (H.mem stops_new v) && is_in_surface g v then begin (* not a street vertices, and not underground *)
        (* find closest point *)
        let _, lat, lng = stops v in
        let xy = project (lat, lng) in
        let (_, closest), dist = Quadtree.closest qt xy in
        let t = int_of_float (dist *. 6. /. 10.) in (* dist in in meters *)
        add_label g v closest (Label.create [] (Some t) v closest);
        add_label g closest v (Label.create [] (Some t) closest v);
        incr tot_surface
      end
    )
    g;
  Printf.printf "%d vertices connected\n" !tot_surface;
  (* contract node dimacs of degree 2 or 4 *)
  stat g;
  let tot_contracted = ref 0 in
  let contract v =
    incr tot_contracted;
    G.iter_pred_e
      (fun pred ->
        assert(Label.ty !(G.E.label pred) = (false, true));
        G.iter_succ_e
          (fun succ ->
            assert(Label.ty !(G.E.label succ) = (false, true));
            if G.E.src pred <> G.E.dst succ then
              add_label g (G.E.src pred) (G.E.dst succ) (Label.link !(G.E.label pred) !(G.E.label succ));
          )
          g v
      )
      g v;
    G.remove_vertex g v
  in
  H.iter
    (fun v _ ->
      if G.in_degree g v = 1 && G.out_degree g v = 1 then begin
        contract v
      end
      else if G.in_degree g v = 2 && G.out_degree g v = 2 then begin
        match G.pred g v, G.succ g v with
        | [p1; p2], [s1; s2] ->
            if p1 = s1 && p2 = s2 then
              contract v
            else if p1 = s2 && p2 = s1 then
              contract v
        | _ -> assert false
      end
    )
    stops_new;
  Printf.printf "%d vertices contracted (middle of the road)\n" !tot_contracted;
  fun v ->
    try
      stops v
    with
      Not_found -> H.find stops_new v

let save chan g =
  let nb_v = G.nb_vertex g in
  output_binary_int chan nb_v;
  G.iter_vertex (fun v -> Gtfs.stop_id_save chan v) g;
  let nb_e = G.nb_edges g in
  output_binary_int chan nb_e;
  G.iter_edges_e
    (fun e ->
      Gtfs.stop_id_save chan (G.E.src e);
      Gtfs.stop_id_save chan (G.E.dst e);
      Label.save chan !(G.E.label e)
    )
    g

let load_from chan =
  let nb_v = input_binary_int chan in
  let g = G.create ~size:nb_v () in
  for i = 1 to nb_v do
    G.add_vertex g (stop_id_load chan)
  done;
  let nb_e = input_binary_int chan in
  for i = 1 to nb_e do
    let src = stop_id_load chan in
    let dst = stop_id_load chan in
    let label = Label.load chan in
    G.add_edge_e g (G.E.create src (ref label) dst)
  done;
  g

(** Copy creating new references *)
let deep_copy g =
  let g' = G.create ~size:(G.nb_vertex g) () in
  G.iter_vertex (G.add_vertex g') g;
  G.iter_edges_e
    (fun e ->
      G.add_edge_e g'
        (G.E.create (G.E.src e) (ref !(G.E.label e)) (G.E.dst e))
    )
    g;
  g'
