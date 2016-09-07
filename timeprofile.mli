val time_profile :
  Transportgraph.G.t ->
  Transportgraph.G.V.t -> Label.t Transportgraph.H.t
val bi_time_profile :
  Transportgraph.G.t ->
  (Transportgraph.G.V.t -> Transportgraph.G.V.t -> bool) ->
  Transportgraph.G.V.t ->
  Transportgraph.G.V.t -> Label.t
val bi_time_profile_dp :
  Transportgraph.G.t ->
  (Transportgraph.G.V.t -> Transportgraph.G.V.t -> bool) ->
  Transportgraph.G.V.t ->
  Transportgraph.G.V.t -> Label.t
val bi_time_profile_labels :
  Label.t Transportgraph.H.t Transportgraph.H.t *
  Label.t Transportgraph.H.t Transportgraph.H.t ->
  Transportgraph.G.V.t ->
  Transportgraph.G.V.t -> Label.t
val label :
  Transportgraph.G.t ->
  int Transportgraph.H.t ->
  Label.t Transportgraph.H.t Transportgraph.H.t *
  Label.t Transportgraph.H.t Transportgraph.H.t
val load :
  in_channel ->
  Label.t Transportgraph.H.t Transportgraph.H.t
val save :
  out_channel ->
  Label.t Transportgraph.H.t Transportgraph.H.t -> unit
