(* eval returns a function *)

let rec tak x y z =
  if y < x then tak (tak (x - 1) y z) (tak (y - 1) z x) (tak (z - 1) x y) else z

(* we eval right to get matched to () *)
let () =
  let print n = print_endline (string_of_int n) in
  print (tak 1 2 3) ;
  print (tak 5 23 7) ;
  print (tak 9 1 0) ;
  print (tak 1 1 1) ;
  print (tak 0 42 0) ;
  print (tak 23498 98734 98776)

(* ocamlopt *.ml && ./a.out *)
