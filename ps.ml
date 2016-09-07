open Transportgraph

let a4x, a4y = 595., 842.
let cx, cy = 2.4, 48.8
let scale = 100. (* m/pt *)
let r = 6400000. (* radius in m *)
let pi = 3.1415

(* equirectangular projection *)
let pt_of_coord (e, n) =
  let y = r *. (n -. cy) *. pi /. 180. in
  let x = r *. (e -. cx) *. cos (cy *. pi /. 180.) *. pi /. 180. in
  x /. scale +. a4x /. 2.,
  y /. scale +. a4y /. 2.

let couleur c r g b =
  Printf.fprintf c "%f %f %f setrgbcolor\n" r g b

let point c rayon p =
  let (x, y) = pt_of_coord p in
  Printf.fprintf c "%f %f %f 0 360 arc\nfill\nstroke\n" x y rayon

let linewidth c w =
  Printf.fprintf c "%f setlinewidth\n" w

let droite c p1 p2 =
  let (x1, y1) = pt_of_coord p1 in
  let (x2, y2) = pt_of_coord p2 in
  Printf.fprintf c "%f %f moveto\n%f %f lineto\nstroke\n" x1 y1 x2 y2

let size_color chan = function
  | 0 -> couleur chan 1. 0. 0.; Some 2.
  | 1 -> couleur chan 0. 0. 1.; Some 1.6
  | 2 -> couleur chan 0. 1. 0.; Some 1.2
  | 3 -> couleur chan 1. 0. 1.; Some 0.8
  | 4 -> couleur chan 1. 1. 0.; Some 0.4
  | _ -> None

let ps_of_g chan g stops depth =
  let coords id =
    let _, lat, lng = stops id in
    (lng, lat)
  in
  (* Draw all vertices in black, distringuished vertices in red *)
  let nfv, nfe = ref 0, ref 0 in
  G.iter_vertex
    (fun v ->
      try
        match size_color chan (depth v) with
        | None -> ()
        | Some size -> point chan size (coords v);
      with Not_found -> ()
    )
    g;
  G.iter_vertex
    (fun v ->
      try
        match size_color chan (depth v) with
        | Some _ -> couleur chan 0. 0. 0.;
             point chan 0.2 (coords v)
        | None ->
           begin
             couleur chan 0. 0. 0.;
             point chan 0.01 (coords v)
           end
      with Not_found -> incr nfv
    )
    g;
  (* Draw all edges in gray *)
  couleur chan 0.02 0.02 0.02;
  linewidth chan 0.001;
  G.iter_edges (fun v v' -> try droite chan (coords v) (coords v') with Not_found -> incr nfe) g;
  Printf.printf "%d\tignored vertex (position not known)\n%d\tignored edges\n%!" !nfv !nfe
