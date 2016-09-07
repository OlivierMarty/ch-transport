open Utils
open Gtfs
open Transportgraph

(* CONTRACTION HIERARCHIE *)

let sep_order = ref (H.create 0)

module ContractionPriority =
  struct
    type priority = int * int * float (* depth * level * edge quotient *)
    type t = G.V.t * priority
    let compare (v1, priority1) (v2, priority2) =
      (*compare priority2 priority1 (* bigger element : with smallest values *)*)
      let d1, l1, q1 = priority1 in
      let d2, l2, q2 = priority2 in
      (*compare (10.*.d2 +. float_of_int l2) (10.*.d1 +. float_of_int l1)*)
      let c = compare (-d2, l2, q2) (-d1, l1, q1) in
      if c == 0 then
        compare v1 v2
      else
        c

    let quotient g contracted v =
      let counter v' i =
        if contracted v' then
          i
        else
          i+1
      in
      let counter_loop v' i =
        if contracted v' then
          i
        else if G.mem_edge g v' v then
          i+1
        else
          i
      in
      let deg_in  = G.fold_pred counter g v 0 in
      let deg_out = G.fold_succ counter g v 0 in
      let loop = G.fold_succ counter_loop g v 0 in
      let shortcuts = deg_in * deg_out - loop in
      float_of_int shortcuts (*/. float_of_int (deg_in + deg_out)*)

    let init g v =
      (H.find !sep_order v, 0, quotient g (fun _ -> false) v)

    let compute g order v old nei =
      let old_depth, old_level, _ = old in
      let _, nei_level, _ = nei in
      let level = max old_level (nei_level + 1) in
      let contracted = H.mem order in
      (old_depth, level, quotient g contracted v)

    let print chan priority =
      let depth, level, quotient = priority in
      Printf.fprintf chan "depth=%d\tlevel=%d\tquotient=%f" depth level quotient
  end

module GCH = Ch.Make(ContractionPriority)
module Sep = Separator.Make(G)

(** Compute recursively small separators *)
let make_order g stops =
  let order = H.create (G.nb_vertex g) in
  let comp depth v1 v2 =
    let _, lat1, lng1 = stops v1 in
    let _, lat2, lng2 = stops v2 in
    if depth mod 2 = 1 then
      compare (lat1 -. lng1, v1) (lat2 -. lng2, v2)
    else
      compare (lat1 +. lng1, v1) (lat2 +. lng2, v2)
    in
  let rec aux g depth =
    if G.nb_vertex g <= 10 then (* TODO quelle limite ? quand on trouve pas de petit séparateur ? *)
      G.iter_vertex (fun v -> H.add order v depth) g
    else begin
      let cut = Sep.separator g (comp depth) in
      let gl, gr = G.copy g, G.copy g in
      Sep.H.iter
        (fun v t ->
          if t <> Sep.Component false then
            G.remove_vertex gl v;
          if t <> Sep.Component true then
            G.remove_vertex gr v;
          if t = Sep.Cut then
            H.add order v depth
        )
        cut;
      aux gl (depth+1);
      aux gr (depth+1);
    end
  in
  aux g 0;
  order

let arg_d = ref ""
let arg_gtfs = ref ""
let arg_g = ref ""
let arg_lg = ref false
let arg_cg = ref ""
let arg_lcg = ref false
let arg_s = ref ""
let arg_ls = ref false
let arg_lab = ref false
let arg_l = ref ""
let arg_ll = ref false
let arg_n = ref 100
IFDEF TRIP THEN
  let arg_dim = ref false
ENDIF

let parse_arg () =
  let open Arg in
  let set_and_check str b a1 a2 () =
    if !str = "" then
      raise (Bad ("'" ^ a1 ^ "' requires '" ^ a2 ^ "'"));
    b := true
  in
  let check_date str =
    ignore (str.[0]);
    let code0 = Char.code '0' in
    let code9 = Char.code '9' in
    (* check length *)
    if String.length str <> 8 then
      raise (Bad "bad date format");
    (* check 0-9 *)
    String.iter
      (fun c ->
        let c = Char.code c in
        if c < code0 || c > code9 then
          raise (Bad "bad date format")
      )
      str;
    arg_d := str
  in
  let speclist =
    [
      ("-d", String check_date, " Date in format yyyymmdd (required)");
      ("-gtfs", Set_string arg_gtfs, " Directory of gtfs (required)");
      ("-g", Set_string arg_g, " Path to save / load the graph");
      ("-lg", Unit (set_and_check arg_g arg_lg "-lg" "-g"), " Load the graph (require -g)");
      ("-cg", Set_string arg_cg, " Path to save / load the contracted graph");
      ("-lcg", Unit (set_and_check arg_cg arg_lcg "-lcg" "-cg"), " Load the contracted graph (require -cg)");
      ("-s", Set_string arg_s, " Path to save / load the separators");
      ("-ls", Unit (set_and_check arg_s arg_ls "-ls" "-s"), " Load the separators (require -s)");
      ("-lab", Unit (set_and_check arg_l arg_lab "-lab" "-l"), " Compute/load labels (require -l)");
      ("-l", Set_string arg_l, " Path to save / load the labels");
      ("-ll", Unit (set_and_check arg_l arg_ll "-ll" "-l"), " Load the labels (require -l)");
      ("-n", Set_int arg_n, " Number of tests (default: 100)");
    ]
  in
  let speclist =
    IFDEF TRIP THEN
      align (speclist @ [("-dim", Set arg_dim, " Add streets (if compiled with -DTRIP)")])
    ELSE
      align speclist
    ENDIF
  in
  parse speclist (fun _ -> raise (Bad "no anonymous arguments")) "Usage:";
  if !arg_d = "" then begin
    usage speclist (Sys.argv.(0) ^ ": -d is required\nUsage:");
    exit 2
  end;
  if !arg_gtfs = "" then begin
    usage speclist (Sys.argv.(0) ^ ": -gtfs is required\nUsage:");
    exit 2
  end


let save_or_load ?(f_in=Marshal.from_channel) ?(f_out=fun chan x -> Marshal.to_channel chan x []) file b f =
  if b then begin
    Printf.printf "Load from %s\n%!" file;
    let chan = open_in file in
    let v = f_in chan in
    close_in chan;
    v
  end else begin
    let v = f () in
    if file <> "" then begin
      Printf.printf "Save to %s\n%!" file;
      let chan = open_out file in
      f_out chan v;
      close_out chan
    end;
    v
  end


let () =
  (* set GC in aggressive mode *)
  Gc.set { (Gc.get ()) with Gc.space_overhead = 40 };
  Random.self_init ();
  let start = Sys.time () in
  let print_time () = Printf.printf "time : %fs\n" (Sys.time () -. start) in

  parse_arg ();

  print_time ();
  Printf.printf "Load graph...\n%!";
  let f_in chan =
    Gtfs.load chan;
    let g = load_from chan in
    let l = input_binary_int chan in
    let date = really_input_string chan l in
    g, date
  in
  let f_out chan (g, date) =
    Gtfs.save chan;
    save chan g;
    output_binary_int chan (String.length date);
    output_string chan date
  in
  let g, date = save_or_load ~f_in ~f_out !arg_g !arg_lg
    (fun () -> load ~dir:!arg_gtfs ~date:!arg_d, !arg_d)
  in
  (* check the date if the graph was saved *)
  assert(!arg_d = date);
  let stops = Gtfs.stops ~dir:!arg_gtfs in
  let trips = Gtfs.trips ~dir:!arg_gtfs in
  stat g;
  print_time ();
  (*let g_station = G.copy g in
  Printf.printf "Create station graph...\n%!";
  let stops = station_graph g_station stops in
  Test.test_profile !arg_n trips g g_station*)

  let stops =
    IFDEF TRIP THEN
    if !arg_dim then begin
      Printf.printf "Add streets...\n%!";
      let stops = add_dimacs g "ile-de-france.osm-d.gr" "ile-de-france.osm.co" stops trips in
      stat g;
      stops
    end
    else
      stops
    ELSE
      stops
    ENDIF
  in

  Printf.printf "Contracted graph...\n%!";
  let f_in chan =
    let g_contracted, date = f_in chan in
    let nb_v = G.nb_vertex g_contracted in
    let order = H.create nb_v in
    assert(input_binary_int chan = nb_v);
    for i = 1 to nb_v do
      let v = stop_id_load chan in
      H.add order v (input_binary_int chan)
    done;
    g_contracted, date, order
  in
  let f_out chan (g_contracted, date, order) =
    f_out chan (g_contracted, date);
    output_binary_int chan (G.nb_vertex g_contracted);
    G.iter_vertex
      (fun v ->
        stop_id_save chan v;
        output_binary_int chan (H.find order v)
      )
      g_contracted
  in
  let g_contracted, date, order = save_or_load ~f_in ~f_out !arg_cg !arg_lcg
    (fun () ->
      let g_contracted = deep_copy g in
      Printf.printf "Separators...\n%!";
      sep_order := save_or_load !arg_s !arg_ls (fun () -> make_order g stops);
      print_time ();
      (* print stat on signal SIGUSR1 *)
      Sys.set_signal Sys.sigusr1 (Sys.Signal_handle (fun _ -> stat g_contracted));
      let order = GCH.ch g_contracted in
      stat g_contracted;
      g_contracted, !arg_d, order)
  in
  (* check the date if the contracted graph was saved *)
  assert(!arg_d = date);
  stat g_contracted;
  print_time ();

  let comp v v' = compare (H.find order v) (H.find order v') in

  if !arg_lab then begin
    Printf.printf "Labels...\n";
    let f_out chan (l, l') =
      Timeprofile.save chan l;
      Timeprofile.save chan l'
    in
    let f_in chan =
      let l = Timeprofile.load chan in
      (l, Timeprofile.load chan)
    in
    let labels =
      save_or_load ~f_in ~f_out !arg_l !arg_ll
      (fun () -> Timeprofile.label g_contracted order)
    in
    print_time ();

    (* Run tests *)
    Test.test !arg_n g g_contracted comp ~labels
  end
  else
    Test.test !arg_n g g_contracted comp;

  print_time ()
