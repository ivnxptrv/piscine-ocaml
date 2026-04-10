(* eval returnes a function *)
let ft_test_sign n =
  if (n >= 0) then
       print_endline "positive"
  else
       print_endline "negative"

(* we eval right to get matched to () *)
let () =
  ft_test_sign 42;
  ft_test_sign 0;
  ft_test_sign (-42) 
