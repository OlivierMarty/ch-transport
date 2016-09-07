val ps_of_g :
  out_channel ->
  Transportgraph.G.t ->
  (Transportgraph.G.vertex -> 'a * float * float) ->
  (Transportgraph.G.V.t -> int) -> unit
