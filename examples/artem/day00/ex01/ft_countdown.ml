let ft_print_int_newline n =
  print_int n;
  print_char '\n'

let rec ft_countdown n =
  if n <= 0 then ft_print_int_newline 0
  else (
    ft_print_int_newline n;
    ft_countdown (n - 1))

(* Tests *)
let () =
  ft_countdown 10;
  ft_countdown 0;
  ft_countdown (-1)
