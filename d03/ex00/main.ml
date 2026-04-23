(* eval returns a function *)

let print_color color =
  print_endline (Color.toString color ^ " -> " ^ Color.toStringVerbose color)

let () =
  print_color Color.Spade ;
  print_color Color.Heart ;
  print_color Color.Diamond ;
  print_color Color.Club
(* ocamlopt *.ml && ./a.out *)
