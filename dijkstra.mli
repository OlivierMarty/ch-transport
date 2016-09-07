val dijkstra :
  Transportgraph.G.t ->
  Gtfs.time ->
  Transportgraph.G.V.t ->
  Transportgraph.G.V.t ->
  Trip.trip * int

val bi_dijkstra :
  Transportgraph.G.t ->
  (Transportgraph.G.V.t -> Transportgraph.G.V.t -> int) ->
  Gtfs.time ->
  Transportgraph.G.V.t -> Transportgraph.G.V.t ->
  Trip.trip * int
