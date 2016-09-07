type t

val empty : t
val create : Gtfs.connection list -> Gtfs.time option -> Gtfs.stop_id -> Gtfs.stop_id -> t
val get_w : t -> int
IFDEF TRIP THEN
val get_w_hop : t -> Gtfs.info
END
val get_dep : t -> Gtfs.stop_id
val get_arr : t -> Gtfs.stop_id
val iter_c : (Gtfs.connection -> unit) -> t -> unit
val merge : t -> t -> t
val link : ?query:bool -> t -> t -> t
val next_dep : t -> Gtfs.time -> Gtfs.connection option
val size : t -> int
val ty : t -> bool * bool
val mint : t -> int
val maxt : t -> int
val dominate : t -> t -> bool
(*val count_profile :
  ((Gtfs.stop_id * Gtfs.stop_id * Gtfs.connection) list ->
   (Gtfs.stop_id * Gtfs.stop_id * Gtfs.connection) list) ->
  (string * 'a) Utils.StringMap.t -> t -> int * int*)
val save : out_channel -> t -> unit
val load : in_channel -> t
