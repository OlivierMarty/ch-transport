open Gtfs
open Transportgraph

module Elt =
  struct
    type t = int * int * G.V.t * Label.t (* min * update * v * label *)
    let compare (w1, u1, v1, _) (w2, u2, v2, _) =
      let cw = compare w2 w1 in
      if cw != 0 then cw else compare (v2, u2) (v1, u1)
  end

module PQ = Heap.Imperative(Elt)


(** Compute profile from v1 following valid edges, and in the reverse graph if reverse with Bellman-Ford's algorithm *)
let time_profile_gen g v1 valid reverse =
  let dist = H.create 97 in
  let q = PQ.create 17 in
  let iter_next, dst, link =
    if reverse then
      G.iter_pred_e, G.E.src, fun a b -> Label.link ~query:true b a
    else
      G.iter_succ_e, G.E.dst, Label.link ~query:true
  in
  let update = H.create 97 in
  let rec loop () =
    if not (PQ.is_empty q) then begin
      let (w,u,v,l) = PQ.pop_maximum q in
      if u = H.find update v then begin
        iter_next
          (fun e ->
            let v' = dst e in
            if valid v v' then begin
              let lab = link l !(G.E.label e) in
              let old_lab = try Some (H.find dist v') with Not_found -> None in
              let improvement =
                match old_lab with
                | Some lab' -> not (Label.dominate lab' lab)
                | None -> true
              in
              if improvement then begin
                let new_lab =
                  match old_lab with
                  | Some lab' -> Label.merge lab lab'
                  | None -> lab
                in
                H.replace dist v' new_lab;
                let u = try H.find update v' with Not_found -> 0 in
                H.replace update v' (u+1);
                PQ.add q (Label.mint new_lab, u+1, v', new_lab) (* TODO heap avec replace plutot que la hashtbl update ? *)
              end
            end
          )
          g v
      end;
      loop ()
    end
  in
  let lab_dep = Label.create [] (Some 0) v1 v1 in
  PQ.add q (0, 0, v1, lab_dep);
  H.add dist v1 lab_dep;
  H.add update v1 0;
  loop ();
  dist

(** Compute profile from v1 following valid edges, and in the reverse graph if reverse by dynamic programming *)
(* TODO calculer seulement les sommets qui apparaissent dans forward et backward *)
let time_profile_dp g v1 valid reverse =
  let iter_next, iter_next_e, dst, link =
    if reverse then
      G.iter_pred, G.iter_pred_e, G.E.src, fun a b -> Label.link ~query:true b a
    else
      G.iter_succ, G.iter_succ_e, G.E.dst, Label.link ~query:true
  in
  let in_degrees = H.create 97 in
  let dist = H.create 97 in
  (* compute in degrees in our exploration area TODO or use contraction order *)
  let rec dfs v =
    try
      let deg = H.find in_degrees v in
      H.replace in_degrees v (deg+1)
    with Not_found -> begin
      H.add in_degrees v 1;
      iter_next
        (fun v' -> if valid v v' then dfs v')
        g v
    end
  in
  dfs v1;
  assert(H.find in_degrees v1 = 1);
  let rec dfs v =
    let deg = H.find in_degrees v in
    assert(deg > 0);
    if deg > 1 then
      H.replace in_degrees v (deg-1)
    else begin
      H.remove in_degrees v;
      let lab = H.find dist v in
      iter_next_e
        (fun e ->
          let v' = dst e in
          let lab_e = !(G.E.label e) in
          if valid v v' then begin
            let lab' = link lab lab_e in
            begin try
              let lab_next = H.find dist v' in
              if not (Label.dominate lab_next lab') then
                H.replace dist v' (Label.merge lab' lab_next)
            with Not_found -> H.add dist v' lab'
            end;
            dfs v'
          end)
        g v
    end
  in
  let lab_dep = Label.create [] (Some 0) v1 v1 in
  H.add dist v1 lab_dep;
  dfs v1;
  assert (H.length in_degrees = 0);
  dist


let time_profile g v1 =
  time_profile_gen g v1 (fun _ _ -> true) false

(** Intersect two half profiles *)
let intersect forward backward =
  Printf.printf "Forward size : %d\n%!" (H.length forward);
  Printf.printf "Backward size : %d\n%!" (H.length backward);
  let res = H.fold
    (fun v lab res ->
      try
        let lab' = H.find backward v in
        let new_lab = Label.link ~query:true lab lab' in
        begin match res with
        | None -> Some new_lab
        | Some l -> Some (Label.merge l new_lab)
        end
      with Not_found -> res
    )
    forward
    None
  in
  match res with
  | Some lab -> lab
  | None -> raise Not_found

(** Bidirectional computation of time profile
  @param valid : say if an edge v -> v' is upward *)
let bi_time_profile g valid v1 v2 =
  let forward = time_profile_gen g v1 valid false in
  Printf.printf "Forward size : %d\n%!" (H.length forward);
  let backward = time_profile_gen g v2 valid true in
  Printf.printf "Backward size : %d\n%!" (H.length backward);
  intersect forward backward

(** Bidirectional computation of time profile by dynamic programming
  @param valid : say if an edge v -> v' is upward *)
let bi_time_profile_dp g valid v1 v2 =
  let forward = time_profile_dp g v1 valid false in
  Printf.printf "Forward size : %d\n%!" (H.length forward);
  let backward = time_profile_dp g v2 valid true in
  Printf.printf "Backward size : %d\n%!" (H.length backward);
  intersect forward backward

(* TODO cas v1 = v2 *)
(* TODO utiliser intersect *)
let bi_time_profile_labels (labels, labels_rev) v1 v2 =
  let forward = H.find labels v1 in
  Printf.printf "Forward size : %d\n%!" (H.length forward);
  let backward = H.find labels_rev v2 in
  Printf.printf "Backward size : %d\n%!" (H.length backward);
  let merge l l' =
    match l with
    | Some lab -> Some (Label.merge lab l')
    | None -> Some l'
  in
  (* 1 hop *)
  let res = try Some (H.find forward v2) with Not_found -> None in
  let res = try merge res (H.find backward v1) with Not_found -> res in
  (* 2 hops *)
  let res = H.fold
    (fun v lab res ->
      try
        let lab' = H.find backward v in
        let new_lab = Label.link ~query:true lab lab' in
        merge res new_lab
      with Not_found -> res
    )
    forward
    res
  in
  match res with
  | Some lab -> lab
  | None -> raise Not_found

let label g order =
  (* compute the profile in reverse order of contraction : the forward graph is a dag *)
  print_endline "Computing labels...";

  let time_last = ref (Sys.time ()) in
  let time_reset () = time_last := Sys.time () in
  let timer () = Sys.time () -. !time_last in

  let nb_v = G.nb_vertex g in
  let i = ref 0 in
  let last = ref (timer ()) in
  let maxi = ref 0. in

  let vertices = G.fold_vertex (fun v l -> (v, H.find order v) :: l) g [] in
  let vertices = List.sort (fun (v1, o1) (v2, o2) -> compare (o2, v2) (o1, v1)) vertices in

  let add prof v lab =
    try
      let old_lab = H.find prof v in
      let new_lab =
        if Label.dominate old_lab lab then
          old_lab
        else if Label.dominate lab old_lab then
          lab
        else
          Label.merge old_lab lab
      in
      H.replace prof v new_lab
    with Not_found ->
      H.add prof v lab
  in
  let profile res iter_next dst link (v, o) =
    let prof = H.create 0 in
    iter_next
      (fun e ->
        let v' = dst e in
        if H.find order v' > o then begin
          let lab = !(G.E.label e) in
          add prof v' lab;
          H.iter (fun v' lab' -> add prof v' (link lab lab')) (H.find res v')
        end
      )
      g
      v;
    H.add res v prof;
    incr i;
    let t = timer () in
    let current = t -. !last in
    maxi := max current !maxi;
    last := t;
    Printf.printf "%d/%d\t(time tot: %f\tmean:%f\tmax:%f\tlast:%f) label size : %d\n%!" !i (2*nb_v) t (t /. float_of_int !i) !maxi current (H.length prof)
  in
  time_reset ();
  last := timer ();
  let res = H.create nb_v in
  List.iter (profile res G.iter_succ_e G.E.dst (Label.link ~query:true)) vertices;
  last := timer ();
  let res_back = H.create nb_v in
  List.iter (profile res_back G.iter_pred_e G.E.src (fun a b -> Label.link ~query:true b a)) vertices;
  res, res_back

let load chan =
  let n = input_binary_int chan in
  let labels = H.create n in
  for i = 1 to n do
    let v = stop_id_load chan in
    let m = input_binary_int chan in
    let h = H.create m in
    for j = 1 to m do
      let s = stop_id_load chan in
      H.add h s (Label.load chan)
    done;
    H.add labels v h
  done;
  labels

let save chan labels =
  output_binary_int chan (H.length labels);
  H.iter
    (fun v h ->
      stop_id_save chan v;
      output_binary_int chan (H.length h);
      H.iter
        (fun s l ->
          stop_id_save chan s;
          Label.save chan l
        )
        h
    )
    labels
