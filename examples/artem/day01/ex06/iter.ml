let iter f x n =
  let rec iter_aux f x n = if n = 0 then x else iter_aux f (f x) (n - 1) in
  if n < 0 then -1 else iter_aux f x n

(* Tests *)
let () =
  print_int (iter (fun x -> x * x) 2 4);
  print_newline ();
  print_int (iter (fun x -> x * 2) 2 4);
  print_newline ()
