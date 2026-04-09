let crossover lst1 lst2 =
  let rec find_in_list lst x =
    match lst with
    | [] -> false
    | head :: tail -> head = x || find_in_list tail x
  in
  let rec crossover_aux = function
    | [] -> []
    | head :: tail ->
        if find_in_list lst2 head then head :: crossover_aux tail
        else crossover_aux tail
  in
  crossover_aux lst1

(* Tests *)
let () =
  print_string "Test 1: ";
  print_endline (string_of_bool (crossover [ 1; 2; 3 ] [ 2; 3; 4 ] = [ 2; 3 ]));
  print_string "Test 2: ";
  print_endline (string_of_bool (crossover [ 1; 2; 3 ] [ 4; 5; 6 ] = []));
  print_string "Test 3: ";
  print_endline
    (string_of_bool (crossover [ 1; 2; 3 ] [ 1; 2; 3 ] = [ 1; 2; 3 ]));
  print_string "Test 4: ";
  print_endline
    (string_of_bool (crossover [ 1; 2; 3 ] [ 3; 2; 1 ] = [ 1; 2; 3 ]));
  print_string "Test 5: ";
  print_endline
    (string_of_bool (crossover [ 1; 2; 3 ] [ 3; 2; 1; 4 ] = [ 1; 2; 3 ]))
