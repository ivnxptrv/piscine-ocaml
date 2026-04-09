let ft_rot_letter c n =
  if (c < 'a' || c > 'z') && (c < 'A' || c > 'Z') then
    c
  else
    let ascii = int_of_char c in
    let a_ascii = if c >= 'a' then int_of_char 'a' else int_of_char 'A' in
    let i = ascii - a_ascii in
    let rotated = ((i + n) mod 26 + 26) mod 26 + a_ascii in
    char_of_int rotated

let ft_rot_n n s =
  String.map (fun c -> ft_rot_letter c n) s

(* Test *)
let () =
  print_endline (ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz");
  print_endline (ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz");
  print_endline (ft_rot_n 42 "0123456789");
  print_endline (ft_rot_n 2 "OI2EAS67B9");
  print_endline (ft_rot_n 0 "Damned !");
  print_endline (ft_rot_n 42 "");
  print_endline (ft_rot_n 1 "NBzlk qnbjr !");
  print_endline (ft_rot_n (-2) "abcdefghijklmnopqrstuvwxyz")
