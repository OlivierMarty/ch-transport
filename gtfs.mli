type time = int
type stop_id
type trip_id = string

val stop_id_default : stop_id
val stop_id_save : out_channel -> stop_id -> unit
val stop_id_load : in_channel -> stop_id
val stop_id_print : out_channel -> stop_id -> unit
val stop_id_of_str : string -> stop_id
val stop_id_fresh : unit -> stop_id

val print_time : out_channel -> time -> unit

IFDEF TRIP THEN
type info =
  | Walk
  | Transport of trip_id
  | Contract of stop_id
  | Concat of connection * stop_id * connection
and connection =
  {
    dep: time;
    arr: time;
    info: info;
  }
ELSE
type connection =
  {
    dep: time;
    arr: time;
  }
END

val connections : dir:string -> date:string -> ((stop_id * stop_id), connection list) Hashtbl.t
val transfers : dir:string -> (stop_id * stop_id * int) list
val stops: dir:string -> stop_id -> (string * float * float)
val trips: dir:string -> (string * int) Utils.StringMap.t

val save : out_channel -> unit
val load : in_channel -> unit
