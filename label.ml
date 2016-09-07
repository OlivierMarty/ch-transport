open Utils
open Gtfs

module Connection =
  struct
    type t = connection
    (* increasing on departure time, then decreasing on arrival time *)
    (* decreasing on arrival time permits to detect that 0->2 is dominated
       by 0->1 and does not change query since all dominated connections
       are deleted *)
    let compare c1 c2 = compare (c1.dep, -c1.arr) (c2.dep, -c2.arr)
  end

IFDEF TRIP THEN
type walk =
  {
    w: int option;
    w_hop: info;
  }
let empty_walk =
  {
    w = None;
    w_hop = Walk;
  }
ELSE
type walk =
  {
    w: int option;
  }
let empty_walk =
  {
    w = None;
  }
END

type t =
  {
    l: connection list;
    a: connection array;
    w: walk;
    dep_p: stop_id;
    arr_p: stop_id;
  }

let empty =
  {
    l = [];
    a = [||];
    w = empty_walk;
    dep_p = Gtfs.stop_id_default;
    arr_p = Gtfs.stop_id_default;
  }

(** Constuct a label *)
let construct l w w_hop dep_p arr_p =
  IFDEF TRIP THEN
  {
    l;
    a = Array.of_list l;
    w = { w; w_hop };
    dep_p;
    arr_p;
  }
  ELSE
  {
    l;
    a = Array.of_list l;
    w = { w };
    dep_p;
    arr_p;
  }
  END

(** Return walk time or raise Invalid_argument *)
let get_w { w } =
  match w.w with
  | Some t -> t
  | None -> raise (Invalid_argument "get_w")

(** Return w_hop (or () if compiled without TRIP) *)
IFDEF TRIP THEN
let get_w_hop { w } = w.w_hop
ELSE
let get_w_hop _ = ()
END

(** Return dep or arr *)
let get_dep l = l.dep_p
let get_arr l = l.arr_p


(** [create conn walk] create a label with timetable connections [conn] and walk time [walk] from [dep] to [arr] *)
let create conn walk dep_p arr_p =
  let sorted = List.sort Connection.compare conn in
  IFDEF TRIP THEN
    construct sorted walk Walk dep_p arr_p
  ELSE
    construct sorted walk () dep_p arr_p
  END


(** Iter over connections *)
let iter_c f { l } = List.iter f l


(** Remove all pareto dominated elements from a sorted list of connections
    Complexity : O(|l|) *)
let filter_pareto l =
  let rec aux stack l =
    match l with
    | [] -> stack
    | c::t ->
        begin match stack with
        | [] -> aux [c] t
        | c'::t' ->
            if c.arr > c'.arr then
              aux (c::stack) t
            else (* c.dep >= c'.dep and c.arr <= c'.arr : c dominates c' *)
              aux t' l
        end
  in
  List.rev (aux [] l)

(** Merge and filter two ordered lists of connections *)
let merge_conn l1 l2 =
  List.merge Connection.compare l1 l2

(** Remove all connections longer than w *)
let cut l w =
  match w with
  | None -> l
  | Some t -> List.filter (fun {dep; arr; _} -> (arr - dep < t)) l

(** Parallel combination *)
let merge ({ l = l1; w = w1; dep_p; arr_p } as lab1) ({ l = l2; w = w2; dep_p = dep_p2; arr_p = arr_p2 } as lab2) =
  assert(dep_p = dep_p2);
  assert(arr_p = arr_p2);
  let w, w_hop =
    match w1.w, w2.w with
    | Some t, None -> Some t, get_w_hop lab1
    | None, Some t -> Some t, get_w_hop lab2
    | Some t1, Some t2 ->
        if t1 < t2 then
          Some t1, get_w_hop lab1
        else
          Some t2, get_w_hop lab2
    | None, None -> None, get_w_hop lab1 (* = Walk or () *)
  in
  let l = cut (filter_pareto (merge_conn l1 l2)) w in
  construct l w w_hop dep_p arr_p


(** Serial combination *)
let link ?(query=false) { l = l1; w = w1; dep_p; arr_p = mid1 } { l = l2; w = w2; dep_p = mid2; arr_p } =
  assert(mid1 = mid2);
  (* walk and walk *)
  let ww, w_hop = match w1.w, w2.w with
    | Some a, Some b ->
        Some (a + b),
        IFDEF TRIP THEN
        if query then
          Concat ({dep=0;arr=a;info=w1.w_hop}, mid1, {dep=0;arr=b;info=w2.w_hop})
        else
          Contract mid1
        ELSE
          ()
        END
    | _ ->
        None,
        IFDEF TRIP THEN
          Walk
        ELSE
          ()
        END
  in
  let rec aux l1 l2 =
    match l1, l2 with
    | [], _
    | _, [] -> []
    | kl1::kl2::tl, kr::tr when kl2.arr <= kr.dep ->
        (* we skip link kl1 + kr because it is dominated by kl2 + kr *)
        (* needed to skip kr in the next matching *)
        aux (kl2::tl) (kr::tr)
    | kl::tl, kr::tr ->
        if kl.arr <= kr.dep then
          IFDEF TRIP THEN
            {
              dep = kl.dep;
              arr = kr.arr;
              info = if query then Concat (kl, mid1, kr) else Contract mid1;
            }
          ELSE
            {
              dep = kl.dep;
              arr = kr.arr;
            }
          END :: aux tl tr
        else
          aux (kl::tl) tr
  in
  (* connection and connection *)
  let cc = aux l1 l2 in
  (* walk and connection *)
  let wc =
    match w1.w with
    | Some t ->
        List.map
          (fun k ->
            IFDEF TRIP THEN
            {
              dep = k.dep - t;
              arr = k.arr;
              info = if query then Concat ({dep=k.dep-t; arr=k.dep; info=w1.w_hop}, mid1, k) else Contract mid1;
            }
            ELSE
            {
              dep = k.dep - t;
              arr = k.arr;
            }
            END
          )
          l2
    | None -> []
  in
  (* connection and walk *)
  let cw =
    match w2.w with
    | Some t ->
        List.map
          (fun k ->
            IFDEF TRIP THEN
            {
              dep = k.dep;
              arr = k.arr + t;
              info = if query then Concat (k, mid1, {dep=k.arr; arr=k.arr+t; info=w2.w_hop}) else Contract mid1;
            }
            ELSE
            {
              dep = k.dep;
              arr = k.arr + t;
            }
            END
          )
          l1
    | None -> []
  in
  let l = cut (filter_pareto (merge_conn cc (merge_conn wc cw))) ww in
  construct l ww w_hop dep_p arr_p


(** Return the next departure after t *)
let next_dep { l; a; w; dep_p; arr_p } t =
  let walk =
    match w.w with
    | Some tw ->
        IFDEF TRIP THEN
        Some
          {
            dep = t;
            arr = t + tw;
            info = w.w_hop;
          }
        ELSE
        Some
          {
            dep = t;
            arr = t + tw;
          }
        END
    | None -> None
  in
  let conn = binary_search a (fun c -> c.dep >= t) in
  min_option (fun a b -> compare a.arr b.arr) walk conn



(** for statistics *)
(** Return the number of connections *)
let size { a; _ } =
  Array.length a

(** Return a tuple (c, w) with c = true iff the label contains at least one transport connection
  and w = true iff the label contains a walking connection *)
let ty { l; w; _ } =
  l <> [], w.w <> None

(** Return the minimum travelling time *)
let mint { l; w; _ } =
  let mi = List.fold_left (fun mi k -> min mi (k.arr - k.dep)) max_int l in
  match w.w with
  | Some t -> min mi t
  | None -> mi

(** Return the maximum travelling time *)
let maxt { w; _ } =
  (*let ma, _ = List.fold_left (fun (ma, old_dep) k -> max ma (k.arr - old_dep), k.dep) (min_int, 0) l in*)
  match w.w with
  | Some t -> t
  | None -> max_int

(** Return true if the first label dominates the second one
    Complexity : O(|l1|+|l2|)
*)
let dominate ({ l=l1; w=w1; dep_p; arr_p; _ } as lab1) { l=l2; w=w2; dep_p=dep_p2; arr_p=arr_p2; _ } =
  assert(dep_p = dep_p2);
  assert(arr_p = arr_p2);
  let max_walk = match w2.w with
  | Some t -> maxt lab1 > t
  | None -> false
  in
  if max_walk then
    false
  else begin
    (* check if every connection in c2 is dominated by a connection in c1 or by w1 *)
    let rec check l1 l2 =
      match l2 with
      | [] -> true
      | c::t ->
          let dom_walk = match w1.w with
          | Some t -> c.arr - c.dep >= t
          | None -> false
          in
          if dom_walk then
           (* this connection is dominated by walking *)
            check l1 t
          else
            begin match l1 with
            | [] -> false
            | c'::t' ->
                if c'.arr > c.arr then
                  (* no other connection can dominate this c *)
                  false
                else if c'.dep >= c.dep then
                  (* c' dominated c *)
                  check (c'::t') t
                else
                  (* we try the next one *)
                  check t' (c::t)
            end
    in
    check l1 l2
  end

(*
let get_lines expander trips trip =
  IFDEF TRIP THEN
  let trip = expander trip in
  let concat name (last, acc) =
    if name <> last then
      name, name ^ " " ^ acc
    else
      name, acc
  in
  let _, r = List.fold_left
    (fun acc (_, _, { info }) ->
      match info with
      | Transport t ->
          let name, _ = StringMap.find t trips in
          concat name acc
      | Walk -> concat "w" acc
      | _ -> failwith "call Label.get_lines with expanded trips"
    )
    ("", "")
    trip
    in
  r
  ELSE
  "no trip information"
  END

(** Return the number of connection and the number of different trip *)
let count_profile expander trips l =
  let set, num = List.fold_left (fun (set, num) x -> StringSet.add (get_lines expander trips [l.dep_p, l.arr_p, x]) set, num+1) (StringSet.empty, 0) l.l in
  num, (StringSet.cardinal set)*)


let save chan ({ l; a; w; dep_p; arr_p } as lab) =
  let out_byte = output_byte chan in
  let out_int = output_binary_int chan in
  let out_info i =
    IFDEF TRIP THEN
    match i with
    | Walk -> out_byte 0
    | Transport trip_id -> out_byte 1; out_int (String.length trip_id); output_string chan trip_id
    | Contract x -> out_byte 2; stop_id_save chan x
    | _ -> assert false
    ELSE
    out_byte 3
    END
  in
  begin match w.w with
    | None -> out_byte 0
    | Some x -> out_byte 1; out_int x
  end;
  out_info (get_w_hop lab);
  stop_id_save chan dep_p;
  stop_id_save chan arr_p;
  out_int (Array.length a);
  List.iter
    (fun c ->
      out_int c.dep;
      out_int c.arr;
      IFDEF TRIP THEN
        out_info c.info;
      ELSE
        out_byte 3;
      END
    )
    l

let load chan =
  let in_byte () = input_byte chan in
  let in_int () = input_binary_int chan in
  let in_info () =
    IFDEF TRIP THEN
    match in_byte () with
    | 0 -> Walk
    | 1 ->
        let len = in_int () in
        let trip_id = really_input_string chan len in
        Transport trip_id
    | 2 -> Contract (stop_id_load chan)
    | 3 -> prerr_string "WARNING: loading a graph without trip information !"; Walk
    | _ -> assert false
    ELSE
    match in_byte () with
    | 0
    | 3 -> ()
    | 1 ->
        let len = in_int () in
        let _ = really_input_string chan len in ()
    | 2 -> let _ = stop_id_load chan in ()
    | _ -> assert false
    END
  in
  let w = match in_byte () with
    | 0 -> None
    | 1 -> Some (in_int ())
    | _ -> assert false
  in
  let w_hop = in_info () in
  let dep_p = stop_id_load chan in
  let arr_p = stop_id_load chan in
  let len = in_int () in
  let l = ref [] in
  for i = 1 to len do
    let dep = in_int () in
    let arr = in_int () in
    let info = in_info () in
    IFDEF TRIP THEN
    l := { dep; arr; info } :: (!l)
    ELSE
    l := { dep; arr } :: (!l)
    END
  done;
  let l = List.rev !l in
  construct l w w_hop dep_p arr_p

(* (* TESTS *)

let print (c, w) =
  print_string "conn: ";
  Bst.iter (fun x _ -> Printf.printf "%d/%d " x.dep x.arr) c;
  print_string "\nwalk: ";
  begin match w with
  | Some t -> print_int t
  | None -> print_string "/"
  end;
  print_newline ()

let create_conn d a = { dep = d; arr = a }, ()

let test_pareto () =
  let l = [
    create_conn 0 2;
    create_conn 0 1;
    create_conn 1 6;
    create_conn 1 5;
    create_conn 2 6;
    create_conn 3 4;
    create_conn 4 8;
    create_conn 5 7;
  ] in
  print (create l None);
  print (create (filter_pareto l) None)

let test_link () =
  let l1 = create [
    create_conn 0 1;
    create_conn 3 4;
  ] (Some 10) in
  let l2 = create [
    create_conn 0 1;
    create_conn 2 3;
    create_conn 3 4;
    create_conn 5 6;
    create_conn 6 7;
  ] (Some 1) in
  print l1;
  print l2;
  print (link l1 l2);
  print (link l2 l1)

let test_merge () =
  let l1 = create [
    create_conn 1 3;
    create_conn 3 6;
    create_conn 7 8;
  ] (Some 10) in
  let l2 = create [
    create_conn 0 1;
    create_conn 2 3;
    create_conn 3 4;
    create_conn 5 6;
    create_conn 6 7;
  ] (Some 5) in
  print l1;
  print l2;
  print (merge l1 l2)

let () =
  print_endline "pareto :";
  test_pareto ();
  print_endline "link :";
  test_link ();
  print_endline "merge :";
  test_merge ()
*)
