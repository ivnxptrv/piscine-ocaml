(* eval returns a function *)

let repeat_x n =
  if n < 0 then "Error"
  else
    let rec build_string n string =
      if n = 0 then string else build_string (n - 1) (string ^ "x")
    in
    build_string n ""

(* we eval right to get matched to () *)
let () =
  print_endline (repeat_x (-1)) ;
  print_endline (repeat_x 0) ;
  print_endline (repeat_x 1) ;
  print_endline (repeat_x 2) ;
  print_endline (repeat_x 5) ;
  print_endline (repeat_x 50)

(* ocamlopt *.ml && ./a.out *)
