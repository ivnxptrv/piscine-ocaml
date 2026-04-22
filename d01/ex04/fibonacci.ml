(* eval returns a function *)

(* let rec fibonacci n = *)
(*   if n < 0 then -1 *)
(*   else if n = 0 then 0 *)
(*   else if n = 1 then 1 *)
(*   else fibonacci (n - 2) + fibonacci (n - 1) *)

(* eval returns a function *)

let fibonacci target =
  let rec calc current_num next_num steps_left =
    if steps_left = 0 then current_num
    else calc next_num (current_num + next_num) (steps_left - 1)
  in
  if target < 0 then -1 else calc 0 1 target

(* we eval right to get matched to () *)
let () =
  let print n = print_endline (string_of_int n) in
  print (fibonacci (-42)) ;
  print (fibonacci 1) ;
  print (fibonacci 3) ;
  print (fibonacci 6) ;
  print (fibonacci 0) ;
  print (fibonacci 50)

(* ocamlopt *.ml && ./a.out *)
(* 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55 *)
