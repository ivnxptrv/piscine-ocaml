let ft_string_all pred s =
  let len = String.length s in
  let rec string_all_rec i =
    if i >= len then true
    else if pred (String.get s i) then string_all_rec (i + 1)
    else false
  in
  string_all_rec 0

(*Test*)
let print_bool b =
  if b then print_endline "true"
  else print_endline "false"

let () =
  let is_digit c = c >= '0' && c <= '9'
  in
  print_bool (ft_string_all is_digit "0123456789");
  print_bool (ft_string_all is_digit "O12EAS67B9")
