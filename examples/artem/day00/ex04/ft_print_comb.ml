let ft_print_comb () =
  let rec recurse x y z =
    if x > 9 then ()
    else if y > 9 then recurse (x + 1) (x + 2) (x + 3)
    else if z > 9 then recurse x (y + 1) (y + 2)
    else begin
      print_int x;
      print_int y;
      print_int z;
      if x < 7 then print_string ", ";
      recurse x y (z + 1)
    end
  in
  recurse 0 1 2;
  print_string("\n")

(* Test *)
let () = ft_print_comb ()
