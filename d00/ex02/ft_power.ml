(* eval returns a function *)
let ft_power =
  fun n ->
    fun p -> (
    let rec pow =
      fun c -> (
        if c = 0 then 1
        else (n * pow (c - 1)))
  in
    pow p)
    

(* we eval right to get matched to () *)
let () =
  print_int (ft_power 2 4); print_char '\n';
  print_int (ft_power 3 0); print_char '\n';
  print_int (ft_power 0 5); print_char '\n';
  
