type coord = float * float
type box = coord * float
type 'a point = coord * 'a
type 'a t

val create : box -> 'a t
val insert : 'a t -> 'a point -> 'a t
val range : 'a t -> box -> 'a point list
val closest : 'a t -> float * float -> 'a point * float
