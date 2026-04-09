let ft_print_comb2 () =
  let rec print_comb2 i j =
  if i >= 100 then ()
  else if j >= 100 then print_comb2 (i + 1) (i + 2)
  else begin
    print_int (i / 10);
    print_int (i mod 10);
    print_string " ";
    print_int (j / 10);
    print_int (j mod 10);
    if i < 98 then print_string ", ";
    print_comb2 i (j + 1)
  end
  in
  print_comb2 0 1;
  print_char '\n'

(* Tests *)
let () = ft_print_comb2 ()
