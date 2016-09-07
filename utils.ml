module Int =
  struct 
    type t = int
    let compare = compare
  end
module IntMap = Map.Make(Int)
module IntSet = Set.Make(Int)

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

let min_option compare a b =
  match a, b with
  | None, None -> None
  | Some v, None
  | None, Some v -> Some v
  | Some va, Some vb ->
      if compare va vb < 0 then
        Some va
      else
        Some vb

(** Return the first element satisfying pred *)
let binary_search a pred =
  let rec aux l r =
    if l = r then begin
      if pred a.(l) then
        Some a.(l)
      else
        None
    end
    else begin
      let m = l + (r-l)/2 in
      if pred a.(m) then
        aux l m
      else
        aux (m+1) r
    end
  in
  match Array.length a with
  | 0 -> None
  | n -> aux 0 (n - 1)
