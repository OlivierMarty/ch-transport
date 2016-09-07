open Utils
open Gtfs
open Transportgraph

(* CONTRACTION HIERARCHIE *)

module type ContractionPriorityS =
  sig
    type priority
    type t = G.V.t * priority
    val compare : t -> t -> int (** the bigger element is contracted before*)
    val init : G.t -> G.V.t -> priority
    val compute : G.t -> int H.t -> G.V.t -> priority -> priority -> priority (** old priority -> contracted node (neighbour) priority -> new priority *)
    val print : out_channel -> priority -> unit
  end



module Make(CP: ContractionPriorityS) =
  struct
    module PQC = Heap.Imperative(CP)

    (** Contract v in g
      @param contracted [contracted v'] indicates that [v'] must be ignored
    *)
    let contract g contracted v =
      let aux pred succ =
        let src = G.E.src pred in
        let dst = G.E.dst succ in
        (* if src = dst : no shortcut *)
        if src <> dst && not (contracted src) && not (contracted dst) then begin
          let link = Label.link !(G.E.label pred) !(G.E.label succ) in
          (* check if shortcut is needed : the edge exists and dominates link *)
          let needed =
            try
              let e = G.find_edge g src dst in
              not (Label.dominate !(G.E.label e) link)
            with Not_found -> true
          in
          if needed then
            add_label g src dst link
        end
      in
      (* for all predecessors and successors call aux *)
      G.iter_pred_e (fun pred -> G.iter_succ_e (aux pred) g v) g v


    (** Return the order of contraction *)
    let ch g  =
      let nb = G.nb_vertex g in
      let values = Hashtbl.create 17 in
      let q = PQC.create 17 in
      let order = H.create 17 in (* order of contraction, for dijkstra *)
      let i = ref 1 in
      (* init *)
      G.iter_vertex
        (fun v ->
          let priority = CP.init g v in
          Hashtbl.add values v priority;
          PQC.add q (v, priority)
        )
        g;
      let rec loop () =
        if not (PQC.is_empty q) then begin
          let v, priority = PQC.pop_maximum q in
          let priority' = Hashtbl.find values v in
          (* if priority <> priority' : it has been updated, we ignore this vertex for the moment *)
          (* TODO delete old value in the heap ? *)
          (* TODO ou calculer paresseusement ? -> mettre les priorités dans une table de hash, et lorsqu'on prend le sommet du tas, on recalcule sa priorité, s'il n'est plus le minimum on le réinsère et on recommence (idée utilisée dans le premier CH) *)
          if priority = priority' && not (H.mem order v) then begin
            H.add order v !i;
            Printf.printf "%d/%d\t(%a)\theap size=%d\n%!" !i nb CP.print priority (PQC.size q);
            incr i;
            contract g (H.mem order) v;
            (* update priorities *)
            let updated = H.create 17 in (* ensure we update only once each node, even if it a successor and a predecessor *)
            let update v' =
              if not (H.mem order v') && not (H.mem updated v') then begin
                H.add updated v' ();
                let old_priority = Hashtbl.find values v' in
                let priority = CP.compute g order v' old_priority priority in
                Hashtbl.replace values v' priority;
                PQC.add q (v', priority)
              end
            in
            G.iter_succ update g v;
            G.iter_pred update g v;
            (* TODO plus rapide en supprimant les sommets contractés(les
            arretes incidentes sont supprimées) -> travailler sur deux graphes ? *)
          end;
          loop ()
        end
      in
      loop ();
      order
  end
