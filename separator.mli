module Make :
  functor (G : Graph.Sig.G) ->
    sig
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
      type vt = Component of bool | Cut
      val separator : G.t -> ?b:float -> (G.vertex -> G.vertex -> int) -> vt H.t
    end
