(* eval returns a function *)
(* x < y < z it pevents duplicates as 012 and 021 *)

let ft_print_comb =
 fun () ->
  let rec next x y z =
    if x > 7 then ()
    else if y > 9 then next (x + 1) (x + 2) (x + 3)
    else if z > 9 then next x (y + 1) (y + 2)
    else begin
      print_int x ;
      print_int y ;
      print_int z ;
      if x < 7 then print_string ", " ;
      next x y (z + 1)
    end
  in
  next 0 1 2 ; print_string "\n"

(* we eval right to get matched to () *)
let () = ft_print_comb ()

(* ocamlopt *.ml && ./a.out *)
