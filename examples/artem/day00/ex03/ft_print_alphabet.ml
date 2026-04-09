let ft_print_alphabet () =
  let rec print_from c =
    if c <= 'z' then begin
      print_char c;
      print_from (char_of_int (int_of_char c + 1))
    end
  in
  print_from 'a';
  print_char '\n'

(* Test *)
let () = ft_print_alphabet ()
