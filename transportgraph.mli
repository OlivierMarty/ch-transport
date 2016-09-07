module Vertex :
  sig
    type t = Gtfs.stop_id
    val compare : 'a -> 'a -> int
    val hash : 'a -> int
    val equal : 'a -> 'a -> bool
  end
module Edge :
  sig
    type t = Label.t ref
    val compare : 'a -> 'a -> int
    val equal : 'a -> 'a -> bool
    val default : t
  end
module G :
  sig
    type t =
        Graph.Imperative.Digraph.ConcreteBidirectionalLabeled(Vertex)(Edge).t
    module V :
      sig
        type t = Vertex.t
        val compare : t -> t -> int
        val hash : t -> int
        val equal : t -> t -> bool
        type label = Vertex.t
        val create : label -> t
        val label : t -> label
      end
    type vertex = V.t
    module E :
      sig
        type t = Vertex.t * Edge.t * Vertex.t
        val compare : t -> t -> int
        type vertex = Vertex.t
        val src : t -> vertex
        val dst : t -> vertex
        type label = Edge.t
        val create : vertex -> label -> vertex -> t
        val label : t -> label
      end
    type edge = E.t
    val is_directed : bool
    val is_empty : t -> bool
    val nb_vertex : t -> int
    val nb_edges : t -> int
    val out_degree : t -> vertex -> int
    val in_degree : t -> vertex -> int
    val mem_vertex : t -> vertex -> bool
    val mem_edge : t -> vertex -> vertex -> bool
    val mem_edge_e : t -> edge -> bool
    val find_edge : t -> vertex -> vertex -> edge
    val find_all_edges : t -> vertex -> vertex -> edge list
    val succ : t -> vertex -> vertex list
    val pred : t -> vertex -> vertex list
    val succ_e : t -> vertex -> edge list
    val pred_e : t -> vertex -> edge list
    val iter_vertex : (vertex -> unit) -> t -> unit
    val fold_vertex : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
    val iter_edges : (vertex -> vertex -> unit) -> t -> unit
    val fold_edges : (vertex -> vertex -> 'a -> 'a) -> t -> 'a -> 'a
    val iter_edges_e : (edge -> unit) -> t -> unit
    val fold_edges_e : (edge -> 'a -> 'a) -> t -> 'a -> 'a
    val map_vertex : (vertex -> vertex) -> t -> t
    val iter_succ : (vertex -> unit) -> t -> vertex -> unit
    val iter_pred : (vertex -> unit) -> t -> vertex -> unit
    val fold_succ : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
    val fold_pred : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
    val iter_succ_e : (edge -> unit) -> t -> vertex -> unit
    val fold_succ_e : (edge -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
    val iter_pred_e : (edge -> unit) -> t -> vertex -> unit
    val fold_pred_e : (edge -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
    val create : ?size:int -> unit -> t
    val clear : t -> unit
    val copy : t -> t
    val add_vertex : t -> vertex -> unit
    val remove_vertex : t -> vertex -> unit
    val add_edge : t -> vertex -> vertex -> unit
    val add_edge_e : t -> edge -> unit
    val remove_edge : t -> vertex -> vertex -> unit
    val remove_edge_e : t -> edge -> unit
  end
module H :
  sig
    type key = G.V.t
    type 'a t = 'a Hashtbl.Make(G.V).t
    val create : int -> 'a t
    val clear : 'a t -> unit
    val reset : 'a t -> unit
    val copy : 'a t -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val find : 'a t -> key -> 'a
    val find_all : 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length : 'a t -> int
    val stats : 'a t -> Hashtbl.statistics
  end

type t = G.t
val load : dir:string -> date:string -> t
val stat : t -> unit
val add_label : t -> G.vertex -> G.vertex -> Label.t -> unit
val contract_stations : t -> (G.V.t -> G.V.t)
val station_graph : t -> (Gtfs.stop_id -> string * float * float) -> Gtfs.stop_id -> string * float * float
val add_dimacs : t -> string -> string ->
  (Gtfs.stop_id -> string * float * float) ->
  (string * int) Utils.StringMap.t ->
  Gtfs.stop_id -> string * float * float
val save : out_channel -> t -> unit
val load_from : in_channel -> t
val deep_copy : t -> t
