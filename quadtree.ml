(** Simple quad trees with at most one point per leaf *)

type coord = float * float (** x, y *)
type box = coord * float   (** center, radius *)

type 'a point = coord * 'a
type 'a node =
  {
    nw: 'a t;
    sw: 'a t;
    ne: 'a t;
    se: 'a t;
  }
and 'a tree = Node of 'a node | Point of 'a point | Empty
and 'a t =
  {
    box: box;
    tree: 'a tree;
  }

(** create a quad tree *)
let create box =
  {
    box;
    tree = Empty;
  }

let subdivide box =
  let (bx, by), br = box in
  let br2 = br /. 2. in
  let node =
  {
    nw = create ((bx-.br2, by+.br2), br2);
    sw = create ((bx-.br2, by-.br2), br2);
    ne = create ((bx+.br2, by+.br2), br2);
    se = create ((bx+.br2, by-.br2), br2);
  }
  in
  {
    box;
    tree = Node node;
  }

(** bottom and left edges are inside the box *)
let contains box coord =
  let x, y = coord in
  let (bx, by), br = box in
  x >= bx -. br && x < bx +. br && y >= by -. br && y < by +. br

(** insert a point in a quad tree *)
let rec insert qt point =
  (*assert(contains qt.box (fst point));*)
  match qt.tree with
  | Empty -> { qt with tree = Point point }
  | Node quad ->
      let x, y = fst point in
      let bx, by = fst qt.box in
      { qt with tree = match x < bx, y < by with
        | true,  true  -> Node { quad with sw = insert quad.sw point }
        | true,  false -> Node { quad with nw = insert quad.nw point }
        | false, true  -> Node { quad with se = insert quad.se point }
        | false, false -> Node { quad with ne = insert quad.ne point }
      }
  | Point p ->
      if fst p = fst point then begin
        Printf.eprintf "WARNING Quadtree.insert: two points have the same coordinates, ignoring one of them\n";
        qt
      end
      else
        let qt = subdivide qt.box in
        insert (insert qt p) point

(** all edges are inside the boxes *)
let intersect box box' =
  let (bx, by), br = box in
  let (bx', by'), br' = box' in
  bx -. br <= bx' +. br'
  && bx +. br >= bx' -. br'
  && by -. br <= by' +. br'
  && by +. br >= by' -. br'

(** find all points in a range *)
let range qt box =
  let rec aux qt box acc =
    match qt.tree with
    | Empty -> acc
    | Point ((coord, _) as p) ->
        if contains box coord then
          p :: acc
        else
          acc
    | Node { nw; sw; ne; se } ->
        if intersect box qt.box then
          aux nw box acc
          |> aux sw box
          |> aux ne box
          |> aux se box
        else
          acc
  in
  aux qt box []

let distance (x, y) (x', y') =
  let dx = x -. x' in
  let dy = y -. y' in
  dx *. dx +. dy *. dy

(* TODO do clever *)
(** find the closest point
    @return the point and its distance from the queried point
*)
let closest qt coord =
  if qt.tree = Empty then
    raise Not_found;
  let rec find_max_radius r =
    match range qt (coord, r) with
    | [] -> find_max_radius (2. *. r)
    | _ -> r
  in
  let rec dicho rmax rmin =
    let r = (rmax +. rmin) /. 2. in
    match range qt (coord, r) with
    | [] -> dicho rmax r
    | l ->
        if List.length l <= 50 then r
        else dicho r rmin
    (*| _::[] -> r
    | _ -> dicho r rmin*)
  in
  (* find a radius that contains at least a point *)
  let rmax = find_max_radius (snd qt.box /. 1024.) in
  (* find a radius that contains few points *)
  let r = dicho rmax 0. in
  (* find all points into 2r (the closest is among them) *)
  let l = range qt (coord, 2. *. r) in
  (* find the closest point in l *)
  let p = List.hd l in
  let d = distance coord (fst p) in
  let closest, dist2 = List.fold_left
    (fun (closest, dist) (coord', x) ->
      let dist' = distance coord coord' in
      if dist' < dist then
        (coord', x), dist'
      else
        (closest, dist)
    )
    (p, d)
    (List.tl l)
  in
  closest, sqrt dist2

(*let draw qt x x' =
  let open Graphics in
  let w, h = 800, 800 in
  let (bx, by), br = qt.box in
  let pt x y =
    int_of_float ((x -. bx +. br) /. (2. *. br) *. float_of_int w),
    int_of_float ((y -. by +. br) /. (2. *. br) *. float_of_int h)
  in
  let draw_box box =
    let (bx, by), br = box in
    let x, y = pt (bx-.br) (by-.br) in
    let x', y' = pt (bx+.br) (by+.br) in
    draw_rect x y (x'-x) (y'-y)
  in
  let draw_point (x, y) =
    let x, y = pt x y in
    draw_circle x y 2
  in
  let rec aux qt =
    draw_box qt.box;
    match qt.tree with
    | Empty -> ()
    | Point ((coord, _) as p) ->
        if p = x' then
          set_color green;
        draw_point coord;
        set_color foreground
    | Node { nw; sw; ne; se } ->
        aux nw;
        aux sw;
        aux ne;
        aux se
  in
  open_graph "";
  resize_window w h;
  aux qt;
  set_color red;
  draw_point x;
  ignore (wait_next_event [Button_down; Key_pressed]);
  close_graph ()

let () =
  Random.self_init ();
  let qt = ref (create ((1., 1.), 1.))in
  for i = 0 to 50 do
    qt := insert !qt ((Random.float 2., Random.float 2.), ())
  done;
  let p = Random.float 2., Random.float 2. in
  let p', _ = closest !qt p in
  draw !qt p p'*)
