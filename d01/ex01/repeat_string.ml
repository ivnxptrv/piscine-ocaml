(* eval returns a function *)

let repeat_string ?(str = "x") n =
  if n < 0 then "Error"
  else if n = 0 then ""
  else
    let rec build_string n string =
      if n = 1 then string else build_string (n - 1) (string ^ str)
    in
    build_string n str

(* we eval right to get matched to () *)
let () =
  print_endline (repeat_string (-1)) ;
  print_endline (repeat_string 0) ;
  print_endline (repeat_string ~str:"Toto" 1) ;
  print_endline (repeat_string 2) ;
  print_endline (repeat_string ~str:"a" 5) ;
  print_endline (repeat_string ~str:"what" 3)

(* ocamlopt *.ml && ./a.out *)
