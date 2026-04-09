let rec ft_power x n =
  if n < 0 then raise (Invalid_argument "negative exponent")
  else if n = 0 && x = 0 then raise (Invalid_argument "zero to zero")
  else if n = 0 then 1
  else if x = 0 then 0
  else x * ft_power x (n - 1)

(* Tests *)
let ft_print_int_newline n =
  print_int n;
  print_char '\n'

let () =
  ft_print_int_newline (ft_power 2 4);
  ft_print_int_newline (ft_power 3 0);
  ft_print_int_newline (ft_power 0 5);
