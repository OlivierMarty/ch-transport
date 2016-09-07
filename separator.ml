
module Make(G: Graph.Sig.G) =
struct
  module V =
  struct
    type t = Source | Sink | In of G.V.t | Out of G.V.t
    let compare a b =
      match a, b with
      | In a, In b
      | Out a, Out b -> G.V.compare a b
      | _ -> compare a b
    let hash = function
      | In v -> G.V.hash v
      | Out v -> G.V.hash v + 1
      | x -> Hashtbl.hash x
    let equal a b =
      match a, b with
      | In a, In b
      | Out a, Out b -> G.V.equal a b
      | _ -> a = b
  end
  module Flow =
  struct
    type t = Inf | Scalar of int
    type label = t
    let max_capacity l = l
    let min_capacity _ = Scalar 0
    let flow _ = Scalar 0
    let add a b =
      match a, b with
      | Scalar a, Scalar b -> Scalar (a + b)
      | _ -> Inf
    let sub a b =
      match a, b with
      | Scalar a, Scalar b -> Scalar (a - b)
      | Inf, Scalar _ -> Inf
      | _ -> assert false
    let zero = Scalar 0
    let one = Scalar 1
    let default = Inf
    let compare a b =
      match a, b with
      | Scalar a, Scalar b -> compare a b
      | Inf, Scalar _ -> 1
      | Scalar _, Inf -> -1
      | _ -> assert false
    let print c = function
      | Scalar a -> Printf.fprintf c "%d" a
      | Inf -> Printf.fprintf c "inf"
  end
  module G' = Graph.Imperative.Digraph.ConcreteBidirectionalLabeled(V)(Flow)
  module FlowCompute = Graph.Flow.Ford_Fulkerson(G')(Flow)
  module H = Hashtbl.Make(G.V)
  module H' = Hashtbl.Make(V)
  type vt = Component of bool | Cut

  (* Find a min cut corresponding to a max flow *)
  let min_cut g g' flow =
    let marked = H'.create 17 in
    let rec dfs v =
      if not (H'.mem marked v) then begin
        H'.add marked v ();
        (* mark neighbour in the residual graph *)
        G'.iter_succ_e
          (fun e ->
            let f = flow e in
            let v' = G'.E.dst e in
            let cap = Flow.max_capacity (G'.E.label e) in
            if Flow.compare f cap < 0 then (* unsaturated *)
              dfs v'
          )
          g' v;
        G'.iter_pred_e
          (fun e ->
            let v' = G'.E.src e in
            let f = flow e in
            if Flow.compare f Flow.zero > 0 then (* unsaturated backward edge *)
              dfs v'
          )
          g' v
      end
    in
    dfs V.Source;
    (* On the limit marked / non marked ? *)
    let limit v =
      G'.fold_succ (fun v' b -> b || not (H'.mem marked v')) g' v false
    in
    let result = H.create (G.nb_vertex g) in
    G.iter_vertex
      (fun v ->
        if H'.mem marked (V.In v) then begin
          if limit (V.In v) then
            H.add result v Cut
          else
            H.add result v (Component true)
        end
        else
          H.add result v (Component false)
      ) g;
    result


  let separator g ?(b=0.25) comp =
    assert(b <= 0.5);
    let nb_v = G.nb_vertex g in
    let g' = G'.create ~size:(nb_v*2 + 2) () in
    (* create vertices *)
    G'.add_vertex g' V.Source;
    G'.add_vertex g' V.Sink;
    G.iter_vertex
      (fun v ->
        G'.add_vertex g' (V.In v);
        G'.add_vertex g' (V.Out v);
        G'.add_edge_e g' (G'.E.create (V.In v) Flow.one (V.Out v))
      )
      g;
    (* create edges *)
    let add a b = if not (G'.mem_edge g' a b) then G'.add_edge g' a b in
    G.iter_edges
      (fun v v' ->
        add (V.Out v) (V.In v');
        add (V.Out v') (V.In v)
      )
      g;
    (* sort vertices *)
    let vertices = G.fold_vertex (fun v l -> v::l) g [] (* all vertices *)
      |> List.sort comp                                 (* sorted *)
      |> List.mapi (fun i v -> (i+1, v)) in             (* annoted with rank 1..nb_v*)
    let nb_v = float_of_int nb_v in
    (* add edges from source and to sink *)
    List.iter
      (fun (i, v) ->
        let i = float_of_int i in
        if i <= nb_v *. b then
          G'.add_edge g' V.Source (V.In v)
        else if i > nb_v *. (1.-.b) then
          G'.add_edge g' (V.Out v) V.Sink
      )
      vertices;
    (* compute flow *)
    Printf.printf "compute max flow (V=%d\tE=%d)%!" (G'.nb_vertex g') (G'.nb_edges g');
    let flow, f = FlowCompute.maxflow g' V.Source V.Sink in
    Printf.printf "\t= %a\n%!" Flow.print f;
    min_cut g g' flow
end
