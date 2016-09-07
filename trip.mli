type trip = (Gtfs.stop_id * Gtfs.stop_id * Gtfs.connection) list
val expand : Transportgraph.G.t -> trip -> trip
val print :
  (Gtfs.stop_id -> string * 'a * 'b) ->
  (string * int) Utils.StringMap.t ->
  trip -> unit
val print_label :
  Transportgraph.G.t -> (Gtfs.stop_id -> string * 'a * 'b) ->
  (string * int) Utils.StringMap.t -> Label.t -> unit
