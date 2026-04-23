(* eval returns a function *)

let print_value value =
  print_string "toInt: " ;
  print_int (Value.toInt value) ;
  print_newline () ;
  print_endline ("toString: " ^ Value.toString value) ;
  print_endline ("toStringVerbose: " ^ Value.toStringVerbose value) ;
  print_endline ("next: " ^ Value.toString (Value.next value)) ;
  print_endline ("previous: " ^ Value.toString (Value.previous value)) ;
  print_newline ()

let () =
  print_value Value.Jack ;
  print_value Value.T5 ;
  print_value Value.King ;
  try print_value Value.T2
  with _ -> (
    print_newline () ;
    print_endline "error" ;
    print_newline () ;
    try print_value Value.As
    with _ -> print_newline () ; print_endline "error" ; print_newline () )

(* ocamlopt *.ml && ./a.out *)
