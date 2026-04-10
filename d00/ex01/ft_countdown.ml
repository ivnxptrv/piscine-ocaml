(* eval returns a function *)
let rec ft_countdown =
  fun n -> (* type will be int -> unit because type inference from print_int n *)
      if n > 0 then (
        print_int n; 
        print_char '\n';
        ft_countdown(n - 1))
      else (
        print_int 0; 
        print_char '\n')

(* we eval right to get matched to () *)
let () =
  ft_countdown 3;
  ft_countdown 0;
  ft_countdown (-1)
