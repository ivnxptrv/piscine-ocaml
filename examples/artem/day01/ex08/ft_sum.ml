let ft_sum f a b =
  let rec ft_sum_aux i acc =
    if i > b then acc else ft_sum_aux (i + 1) (acc +. f i)
  in
  ft_sum_aux a 0.0

(* Tests *)
let () =
  print_endline (string_of_float (ft_sum (fun i -> float_of_int (i * i)) 1 10))
