open Utils

type time = int
type stop_id = int
type trip_id = string

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

let stop_id_default = -1
let stop_id_save = output_binary_int
let stop_id_load = input_binary_int

(** Open a file as a csv *)
let open_csv filename =
  let channel = open_in filename in
  Csv.of_channel ~has_header:true channel

(**
  [time_of_string "hh:mm:ss"] returns the number of seconds after midnight corresponding to this time
*)
let time_of_string time =
  let fields = Str.split (Str.regexp_string ":") time in
  match List.map int_of_string fields with
  | [h; m; s] -> (h*60+m)*60+s
  | _ -> assert false

let print_time chan t =
  Printf.fprintf chan "%02d:%02d:%02d" (t/3600) (t mod 3600/60) (t mod 60)

(** Return a map trip_id -> service_id *)
(* TODO fusion avec trips *)
let trips_service ~dir =
  let csv_in = open_csv (dir ^ "/trips.txt") in
  let map = Csv.Rows.fold_left
    ~f:(fun map row ->
      let service_id = Csv.Row.find row "service_id" in
      let trip_id = Csv.Row.find row "trip_id" in
      assert(service_id <> "");
      assert(trip_id <> "");
      StringMap.add trip_id service_id map
    )
    ~init:StringMap.empty
    csv_in
  in
  Csv.close_in csv_in;
  map

(** Return the day of the week of a date in the format YYYYMMDD *)
let weekday_of_date date =
  let days = [| "sunday"; "monday"; "tuesday"; "wednesday"; "thursday"; "friday"; "saturday" |] in
  assert(String.length date = 8);
  let date = int_of_string date in
  let y = date / 10000 in
  let m = (date / 100) mod 100 in
  let d = date mod 100 in
  let _, tm = Unix.(mktime
    {tm_sec=0; tm_min=0; tm_hour=0;
     tm_mday=d; tm_mon=m-1; tm_year=y-1900;
     tm_wday=0; tm_yday=0; tm_isdst=false}) in
  let w = days.(tm.Unix.tm_wday) in
  Printf.eprintf "INFO: %d/%d/%d is a %s\n" d m y w;
  w


(** Return a set of service_id containing all services that holds on this date)
  @param date in the format YYYYMMDD
*)
let service ~dir ~date =
  let weekday = weekday_of_date date in
  let date_int = int_of_string date in
  (* calendar (base) *)
  let csv_in = open_csv (dir ^ "/calendar.txt") in
  let set = Csv.Rows.fold_left
    ~f:(fun set row ->
      let service_id = Csv.Row.find row "service_id" in
      let start_date = int_of_string (Csv.Row.find row "start_date") in
      let end_date = int_of_string (Csv.Row.find row "end_date") in
      assert(service_id <> "");
      if date_int >= start_date && date_int <= end_date && Csv.Row.find row weekday = "1" then (* add this service *)
        StringSet.add service_id set
      else
        set
    )
    ~init:StringSet.empty
    csv_in
  in
  Csv.close_in csv_in;
  (* calendat_dates (exceptions) *)
  let csv_in = open_csv (dir ^ "/calendar_dates.txt") in
  let set = Csv.Rows.fold_left
    ~f:(fun set row ->
      let service_id = Csv.Row.find row "service_id" in
      let date' = Csv.Row.find row "date" in
      let exception_type = int_of_string (Csv.Row.find row "exception_type") in
      assert(service_id <> "");
      assert(date' <> "");
      if date = date' then
        if exception_type == 1 then (* add this service *)
          StringSet.add service_id set
        else (* remove this service *)
          StringSet.remove service_id set
      else
        set
    )
    ~init:set
    csv_in
  in
  Csv.close_in csv_in;
  set

(* a hashtbl that maps stop_id (string) to an id (int) *)
let ids, ids_rev = ref (Hashtbl.create 97), ref (Hashtbl.create 97)
let id_next = ref 0

(** Return the stop_id of a string id *)
let stop_id_of_str str =
  try
    Hashtbl.find !ids str
  with Not_found -> begin
    incr id_next;
    Hashtbl.add !ids str !id_next;
    Hashtbl.add !ids_rev !id_next str;
    !id_next
  end

let str_of_stop_id stop_id =
  Hashtbl.find !ids_rev stop_id

let stop_id_fresh () =
  let prev_id = !id_next in
  let name = Printf.sprintf "%%fresh_%d" !id_next in
  let stop_id = stop_id_of_str name in
  assert(prev_id <> !id_next); (* ensure a new vertex has been created *)
  stop_id

let stop_id_print chan stop_id = Printf.fprintf chan "%s" (str_of_stop_id stop_id)

(** Save state along with a graph *)
let save chan =
  output_binary_int chan (Hashtbl.length !ids);
  Hashtbl.iter
    (fun v i ->
      output_binary_int chan (String.length v);
      output_string chan v;
      output_binary_int chan i
    )
    !ids;
  output_binary_int chan !id_next

let load chan =
  let n = input_binary_int chan in
  ids := Hashtbl.create n;
  ids_rev := Hashtbl.create n;
  for i = 1 to n do
    let len = input_binary_int chan in
    let s = really_input_string chan len in
    let v = input_binary_int chan in
    Hashtbl.add !ids s v;
    Hashtbl.add !ids_rev v s;
  done;
  id_next := input_binary_int chan


(**
  builds a 2 levels map trip_id -> stop_sequence -> (stop_id, departure_time, arrival_time)
*)
let connections_map ~dir ~date =
  let service = service ~dir ~date in
  let trips_service = trips_service ~dir in
  let check_date trip_id =
    (* check if this trip id at the right date *)
    StringSet.mem (StringMap.find trip_id trips_service) service
  in
  let csv_in = open_csv (dir ^ "/stop_times.txt") in
  let map = Csv.Rows.fold_left
    ~f:(fun map row ->
      let trip_id = Csv.Row.find row "trip_id" in
      let arrival_time = time_of_string (Csv.Row.find row "arrival_time") in
      let departure_time = time_of_string (Csv.Row.find row "departure_time") in
      let stop_id = Csv.Row.find row "stop_id" in
      let stop_sequence = int_of_string (Csv.Row.find row "stop_sequence") in
      assert(trip_id <> "");
      assert(stop_id <> "");
      let stop_id = stop_id_of_str stop_id in
      if check_date trip_id then begin
        let old_value = try
          StringMap.find trip_id map
          with Not_found -> IntMap.empty
        in
        let value = IntMap.add stop_sequence (stop_id, arrival_time, departure_time) old_value in
        StringMap.add trip_id value map
      end
      else
        map
    )
    ~init:StringMap.empty
    csv_in
  in
  Csv.close_in csv_in;
  map

(** 
  Return a list of connections [tuple stop_id dep * time dep * stop_id arr * time arr * trip_id] as described in [dir]/stop_times.txt
*)
let connections_list ~dir ~date =
  let map = connections_map dir date in
  (* in IntMap.fold the keys are iterated in increasing order i.e. with stop_sequence increasing *)
  let folder_trip trip_id _ (stop_id, arrival_time, departure_time) (l, old) =
    let l = match old with
      | Some (stop_id_old, departure_time_old) ->
          (* Create a connection between the last stop and this one *)
          let connection = (stop_id_old, departure_time_old, stop_id, arrival_time, trip_id) in
          connection :: l
      | None ->
          (* first stop of this trip *)
          l
    in
    (l, Some (stop_id, departure_time))
  in
  let folder trip_id stops l =
    (* Concatenate connections of all trips *)
    let (l, _) = IntMap.fold (folder_trip trip_id) stops (l, None) in
    l
  in
  StringMap.fold folder map []

(** 
  Returns a map stop_id dep * stop_id arr -> (Common.connection * trip_id) list
  as described in [dir]/stop_times.txt
  TODO on passe quand même par deux représentations intérmédiaires :-(
*)
let connections ~dir ~date =
  let l = connections_list dir date in
  let h = Hashtbl.create 17 in
  let iterator (dep_p, dep_t, arr_p, arr_t, trip_id) =
    let old_value = try
      Hashtbl.find h (dep_p, arr_p)
      with Not_found -> []
    in
    let connection =
      IFDEF TRIP THEN
      {
        dep = dep_t;
        arr = arr_t;
        info = Transport trip_id;
      }
      ELSE
      {
        dep = dep_t;
        arr = arr_t;
      }
      END
    in
    let value = connection::old_value in
    Hashtbl.replace h (dep_p, arr_p) value
  in
  List.iter iterator l;
  h

(** Returns a list of walkpaths as described in [dir]/transfers.txt *)
let transfers ~dir =
  let csv_in = open_csv (dir ^ "/transfers.txt") in
  let l = Csv.Rows.fold_left
    ~f:(fun l row ->
      let from_stop_id = Csv.Row.find row "from_stop_id" in
      let to_stop_id = Csv.Row.find row "to_stop_id" in
      let transfer_type = int_of_string (Csv.Row.find row "transfer_type") in
      let min_transfer_time = Csv.Row.find row "min_transfer_time" in
      assert(from_stop_id <> "");
      assert(to_stop_id <> "");
      let from_stop_id = stop_id_of_str from_stop_id in
      let to_stop_id = stop_id_of_str to_stop_id in
      if transfer_type <> 3 then (* 3 : transfer not possible *)
        let t = try int_of_string min_transfer_time with Failure _ -> 0 in
        (from_stop_id, to_stop_id, t)::l
      else
        l
    )
    ~init:[]
    csv_in in
  Csv.close_in csv_in;
  l

(** Return a function stop_id -> (name, lat, lng) *)
let stops ~dir =
  let csv_in = open_csv (dir ^ "/stops.txt") in
  let map = Csv.Rows.fold_left
    ~f:(fun map row ->
      let stop_id = Csv.Row.find row "stop_id" in
      let name = Csv.Row.find row "stop_name" in
      let lat = float_of_string (Csv.Row.find row "stop_lat") in
      let lng = float_of_string (Csv.Row.find row "stop_lon") in
      assert(stop_id <> "");
      assert(name <> "");
      let stop_id = stop_id_of_str stop_id in
      IntMap.add stop_id (name, lat, lng) map
    )
    ~init:IntMap.empty
    csv_in
  in
  Csv.close_in csv_in;
  fun k -> IntMap.find k map


(** Return a map route_id -> (route_short_name, route_type) *)
let routes ~dir =
  let csv_in = open_csv (dir ^ "/routes.txt") in
  let map = Csv.Rows.fold_left
    ~f:(fun map row ->
      let route_id = Csv.Row.find row "route_id" in
      let route_short_name = Csv.Row.find row "route_short_name" in
      let route_type = int_of_string (Csv.Row.find row "route_type") in
      assert(route_id <> "");
      StringMap.add route_id (route_short_name, route_type) map
    )
    ~init:StringMap.empty
    csv_in
  in
  Csv.close_in csv_in;
  map

(** Return a map trip_id -> (route_short_name, route_type) *) 
let trips ~dir =
  let routes = routes dir in
  let csv_in = open_csv (dir ^ "/trips.txt") in
  let map = Csv.Rows.fold_left
    ~f:(fun map row ->
      let route_id = Csv.Row.find row "route_id" in
      let trip_id = Csv.Row.find row "trip_id" in
      assert(route_id <> "");
      assert(trip_id <> "");
      let values = StringMap.find route_id routes in
      StringMap.add trip_id values map
    )
    ~init:StringMap.empty
    csv_in
  in
  Csv.close_in csv_in;
  map


