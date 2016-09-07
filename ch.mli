module type ContractionPriorityS =
  sig
    type priority
    type t = Transportgraph.G.V.t * priority
    val compare : t -> t -> int
    val init : Transportgraph.G.t -> Transportgraph.G.V.t -> priority
    val compute :
      Transportgraph.G.t ->
      int Transportgraph.H.t ->
      Transportgraph.G.V.t -> priority -> priority -> priority
    val print : out_channel -> priority -> unit
  end
module Make :
  functor (CP : ContractionPriorityS) ->
    sig
      val ch : Transportgraph.G.t -> int Transportgraph.H.t
    end
