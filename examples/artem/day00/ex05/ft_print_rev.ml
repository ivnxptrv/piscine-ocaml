let ft_print_rev s =
  let rec print_rev_rec i =
    if i < 0 then ()
    else begin
      print_char (String.get s i);
      print_rev_rec (i - 1)
    end
  in
  print_rev_rec (String.length s - 1)

(* Test *)
let () =
  ft_print_rev "Hello, World!";
  print_newline ()
