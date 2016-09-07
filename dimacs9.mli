exception Syntax_error of string
val parse_gr : (int -> int -> int -> unit) -> in_channel -> unit
val parse_co : (int -> float -> float -> unit) -> in_channel -> unit
