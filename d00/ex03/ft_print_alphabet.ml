(* eval returns a function *)
let ft_print_alphabet =
 fun () ->
  let rec print_from c =
    if c <= 'z' then begin
      print_char c ;
      print_from (char_of_int (int_of_char c + 1))
    end
  in
  print_from 'a' ; print_char '\n'

(* we eval right to get matched to () *)
let () = ft_print_alphabet ()

(* ocamlopt *.ml && ./a.out *)
