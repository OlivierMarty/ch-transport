open Gtfs
open Transportgraph
open Utils

(** Utility over trips *)

type trip = (stop_id * stop_id * connection) list

(** Return the same trip with contraction expanded *)
let expand g trip =
  IFDEF TRIP THEN
  let rec expand_walk u v c dep acc =
    if u = v then
      dep, acc (* In Timeprofile.time_profile_dp we start with a label v1 to v1 *)
    else
      match c.info with
      | Walk ->
          let t = c.arr - c.dep in
          dep+t, (u, v, {dep; arr=dep+t; info=Walk}) :: acc
      | Concat (kl, mid, kr) ->
          let arr, acc = expand_walk u mid kl dep acc in
          expand_walk mid u kr arr acc
      | Contract mid ->
          let lab = !(G.E.label (G.find_edge g u mid)) in
          let w = Label.get_w lab in
          let w_hop = Label.get_w_hop lab in
          let arr, acc = expand_walk u mid {dep=0; arr=w; info=w_hop} dep acc in
          let lab = !(G.E.label (G.find_edge g mid u)) in
          let w = Label.get_w lab in
          let w_hop = Label.get_w_hop lab in
          expand_walk mid v {dep=0; arr=w; info=w_hop} arr acc
      | Transport _ -> assert false
  and expand_conn u v c dep acc =
    let arr, acc = match c.info with
      | Contract w ->
          let arr, acc = expand_edge u w dep acc in
          expand_edge w v arr acc
      | Concat (kl, mid, kr) ->
          let arr, acc = expand_conn u mid kl dep acc in
          expand_conn mid v kr arr acc
      | Walk ->
          expand_walk u v c dep acc
      | _ -> c.arr, (u, v, c)::acc
    in
    arr, acc
  and expand_edge u v dep acc =
    match Label.next_dep !(G.E.label (G.find_edge g u v)) dep with
    | exception Not_found -> failwith "Invalid trip"
    | None -> failwith "Invalid trip"
    | Some c -> expand_conn u v c dep acc
  in
  let rec aux l acc =
    match l with
    | [] -> List.rev acc
    | (u, v, c)::t ->
        let arr, acc = expand_conn u v c c.dep acc in
        assert(arr = c.arr);
        aux t acc
  in
  aux trip []
  ELSE
  trip
  END

let print_stop stops chan id =
  let name, lat, lng = stops id in
  Printf.fprintf chan "%s (%a)" name stop_id_print id

let print_route trips chan trip_id =
  let line, ty = StringMap.find trip_id trips in
  let ty_name = match ty with
  | 0 -> "tram"
  | 1 -> "metro"
  | 2 -> "train"
  | 3 -> "bus"
  | 4 -> "ferry"
  | 5 -> "cable car"
  | 6 -> "suspended cable car"
  | 7 -> "funicular"
  | n -> Printf.sprintf "(unknown code %d)" n
  in
  Printf.fprintf chan "%s %s" ty_name line

(** Print an expanded trip *)
let print stops trips trip =
  let print_stop = print_stop stops in
  let print_route = print_route trips in
  let open Printf in
  let rec printer_trip (u, v, c) =
    IFDEF TRIP THEN
    match c.info with
    | Walk ->
        printf "Walk from %a at %a to %a at %a (dutation %a)\n" print_stop u print_time c.dep print_stop v print_time c.arr print_time (c.arr - c.dep)
    | Transport trip_id ->
        printf "Take %a (trip %s) from %a at %a to %a at %a\n" print_route trip_id trip_id print_stop u print_time c.dep print_stop v print_time c.arr;
    | _ -> assert false
    ELSE
    printf "Go from %a at %a to %a at %a\n" print_stop u print_time c.dep print_stop v print_time c.arr
    END
  in
  List.iter printer_trip trip

(** Compress a trip to keep one connection per trip_id *)
let compress trip =
  IFDEF TRIP THEN
  let rec folder acc (u, v, c) =
    match acc with
    | [] -> [(u, v, c)]
    | (u', v', c') :: t ->
        begin match c.info, c'.info with
        | Walk, Walk when u = v' ->
            (u', v, {dep = c'.dep; arr = c.arr; info = c.info }) :: t
        | Transport trip_id, Transport trip_id' when trip_id = trip_id' && u = v' ->
            (u', v, {dep = c'.dep; arr = c.arr; info = c.info }) :: t
        | _ -> (u, v, c) :: acc
        end
  in
  List.rev (List.fold_left folder [] trip)
  ELSE
  trip
  END

(** Print a label *)
let print_label g stops trips l =
  let open Printf in
  let dep = Label.get_dep l in
  let arr = Label.get_arr l in
  Printf.printf "----------\nProfile from %a to %a\n" stop_id_print dep stop_id_print arr;
  printf "size: %d" (Label.size l);
  Label.iter_c
    (fun x ->
      let trip = expand g [dep, arr, x] |> compress in
      printf "\n%d/%d\n" x.dep x.arr;
      print stops trips trip
    )
    l;
  print_string "\nwalk: ";
  if snd (Label.ty l) then begin
    let t = Label.get_w l in
    let trip =
      IFDEF TRIP THEN
        expand g [dep, arr, {dep = 0; arr = t; info=Walk}]
      ELSE
        [dep, arr, {dep = 0; arr = t}]
      END
    in
    printf "%d\n" t;
    print stops trips trip
  end
  else
    print_string "/";
  print_endline "\n----------"
