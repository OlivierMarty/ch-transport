(* http://www.dis.uniroma1.it/challenge9/format.shtml *)

exception Syntax_error of string

let parse_gr f chan =
  let rec loop l =
    match input_line chan with
    | line ->
        if line = "" then loop (l+1) (* empty line *)
        else
          begin match line.[0] with
          | 'c'    (* comment *)
          | 'p' -> (* problem *)
              loop (l+1)
          | 'a' -> (* arc *)
              begin match Scanf.sscanf line "a %d %d %d%!" (fun a b c -> a, b, c) with
              | u, v, w ->
                  f u v w;
                  loop (l+1)
              | exception Scanf.Scan_failure str -> raise (Syntax_error (Printf.sprintf "line %d: %s" l str))
              | exception End_of_file -> raise (Syntax_error (Printf.sprintf "line %d: not enough fields" l))
              end
          | _ ->
              raise (Syntax_error (Printf.sprintf "line %d: bad first character" l))
          end
    | exception End_of_file -> ()
  in
  loop 1

let parse_co f chan =
  let rec loop l =
    match input_line chan with
    | line ->
        if line = "" then loop (l+1) (* empty line *)
        else
          begin match line.[0] with
          | 'c'    (* comment *)
          | 'p' -> (* problem *)
              loop (l+1)
          | 'v' -> (* vertex *)
              begin match Scanf.sscanf line "v %d %d %d%!" (fun a b c -> a, b, c) with
              | u, x, y ->
                  let x = float_of_int x /. 1000000. in
                  let y = float_of_int y /. 1000000. in
                  f u x y;
                  loop (l+1)
              | exception Scanf.Scan_failure str -> raise (Syntax_error (Printf.sprintf "line %d: %s" l str))
              | exception End_of_file -> raise (Syntax_error (Printf.sprintf "line %d: not enough fields" l))
              end
          | _ ->
              raise (Syntax_error (Printf.sprintf "line %d: bad first character" l))
          end
    | exception End_of_file -> ()
  in
  loop 1

