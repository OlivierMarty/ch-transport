open Utils
open Gtfs
open Transportgraph

module Elt =
  struct
    type t = int * G.V.t * (Gtfs.stop_id * Gtfs.stop_id * connection) list
    let compare (w1, v1, _) (w2, v2, _) =
      let cw = compare w2 w1 in
      if cw != 0 then cw else G.V.compare v1 v2
  end

module PQ = Heap.Imperative(Elt)


(** Modified version of ocamlgraph
   @param valid [valid v v'] say if we can follow edge [v] -> [v']
   @return a not expanded trip
*)
let dijkstra_gen g t v1 v2 valid =
  let visited = H.create 97 in
  let dist = H.create 97 in
  let q = PQ.create 17 in
  let rec loop () =
    if PQ.is_empty q then raise Not_found;
    let (w,v,p) = PQ.pop_maximum q in
    if G.V.equal v v2 then
      List.rev p, w
    else begin
      if not (H.mem visited v) then begin
        H.add visited v ();
        G.iter_succ_e
          (fun e ->
              let ev = G.E.dst e in
              if not (H.mem visited ev) && valid v ev then begin
                match Label.next_dep !(G.E.label e) w with
                | Some c ->
                    let dev = c.arr(*.t*) in
                    let improvement =
                      try dev < (H.find dist ev) with Not_found -> true
                    in
                    if improvement then begin
                      H.replace dist ev dev;
                      PQ.add q (dev, ev, (v, ev, c) :: p)
                    end
                | None -> ()
              end)
          g v
      end;
      loop ()
    end
  in
  PQ.add q (t, v1, []);
  H.add dist v1 t;
  let rep = loop () in
  Printf.printf "Visited nodes : %d\n" (H.length visited);
  rep

(** Dijsktra from v1 to v2 *)
let dijkstra g t v1 v2 =
  Printf.printf "Dijkstra from %a to %a\n%!" stop_id_print v1 stop_id_print v2;
  dijkstra_gen g t v1 v2 (fun _ _ -> true)

(** Pruned bidirectional Dijkstra search
   @param comp at node [v], we observe a neighbour [v'] only
   if [comp v v' <= 0] in forward search (increasing edges)
   if [comp v' v > 0] in backward search (decreasing edges)
*)
(* TODO pour accelerer (d'après le papier CH) :
  faire forward et backward en meme temps, pruner quand le plus petit candidat de la queue est plus loin que le plus court chemin déjà trouvé *)
let bi_dijkstra g comp t v1 v2 =
  Printf.printf "BiDijkstra from %a to %a\n%!" stop_id_print v1 stop_id_print v2;
  (* mark all vertex coaccessible from v2 *)
  let marked = H.create 97 in
  let rec dfs v =
    H.add marked v ();
    G.iter_pred
      (fun v' ->
        if not (H.mem marked v') && comp v' v > 0 then
          dfs v'
      )
      g
      v
  in
  dfs v2;
  Printf.printf "Backward search space : %d\n%!" (H.length marked);
  (* run a dijkstra on increasing edges and marked vertices *)
  let valid v v' =
    comp v v' <= 0 (* increasing *)
    ||
    H.mem marked v' (* marked target *)
  in
  dijkstra_gen g t v1 v2 valid
