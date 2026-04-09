let fibonacci n =
  let rec fib_aux a b n = if n = 0 then a else fib_aux b (a + b) (n - 1) in
  if n < 0 then -1 else fib_aux 0 1 n

(* Tests *)
let () =
  print_int (fibonacci (-42));
  print_newline ();
  print_int (fibonacci 1);
  print_newline ();
  print_int (fibonacci 3);
  print_newline ();
  print_int (fibonacci 6);
  print_newline ()
