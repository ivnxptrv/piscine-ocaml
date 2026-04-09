let converges f x n =
  let rec converges_aux x n =
    if n < 0 then false
    else
      let y = f x in
      if x = y then true else converges_aux y (n - 1)
  in
  converges_aux x n

(* Tests *)
let () =
  print_endline (string_of_bool (converges (( * ) 2) 2 5));
  print_endline (string_of_bool (converges (fun x -> x / 2) 2 3));
  print_endline (string_of_bool (converges (fun x -> x / 2) 2 2))
