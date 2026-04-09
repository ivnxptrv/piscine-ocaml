let ft_is_palindrome s =
  let len = String.length s in
  let rec check_palindrome i =
    if i >= len / 2 then true
    else if String.get s i = String.get s (len - 1 - i) then
      check_palindrome (i + 1)
    else false
  in
  check_palindrome 0

(* Tests *)
let print_bool b =
  if b then print_endline "true"
  else print_endline "false"

let () =
  print_bool (ft_is_palindrome "");
  print_bool (ft_is_palindrome "racecar");
  print_bool (ft_is_palindrome "hello");
  print_bool (ft_is_palindrome "a");
  print_bool (ft_is_palindrome "madam")
