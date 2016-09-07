val test : int -> Transportgraph.G.t -> Transportgraph.G.t ->
  ?labels:Label.t Transportgraph.H.t Transportgraph.H.t *
  Label.t Transportgraph.H.t Transportgraph.H.t ->
  (Transportgraph.G.V.t -> Transportgraph.G.V.t -> int) ->
  unit
(*val test_profile : int -> (string * 'a) Utils.StringMap.t ->
  Transportgraph.G.t -> Transportgraph.G.t -> unit*)
