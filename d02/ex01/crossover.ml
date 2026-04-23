(* eval returns a function *)

type 'a crossover = 'a list -> 'a list -> 'a list

let crossover lst1 lst2 =
  (* returns true of false if elm is inside of the list *)
  let rec isInside elm lst =
    match lst with
    | [] ->
        false
    | head :: tail ->
        if elm = head then true else isInside elm tail
  in
  (* move to next item to be checked *)
  let rec next lst =
    match lst with
    | [] ->
        []
    | head :: tail ->
        if isInside head lst2 then head :: next tail else next tail
  in
  (* main func body *)
  next lst1

(* 1. take head of 1st list *)
(* 2. check if it is in 2nd list *)
(* 3. return true or flase  *)

(* we eval right to get matched to () *)
let print_bool bool = print_string (string_of_bool bool)

let rec print_int_list lst =
  match lst with
  | [] ->
      print_newline ()
  | head :: tail ->
      print_int head ; print_char ' ' ; print_int_list tail

let () =
  let lst1 = [2; 1; 5] in
  let lst2 = [3; 2; 4; 5] in
  let result = crossover lst1 lst2 in
  if result = [2; 5] then print_int_list result
  else print_string "test 1 failed" ;
  let lst1 = [] in
  let lst2 = [3; 2; 4; 5] in
  print_int_list (crossover lst1 lst2) ;
  let lst1 = [1] in
  let lst2 = [] in
  print_int_list (crossover lst1 lst2) ;
  let lst1 = [1; 2; 3] in
  let lst2 = [1; 10; 3; 5] in
  print_int_list (crossover lst1 lst2)

(* match lst1 with *)
(* | [] -> *)
(*     print_bool false *)
(* | head :: tail -> *)
(*     print_bool (isInside head lst2) *)
(* ocamlopt *.ml && ./a.out *)

(* return list of all common elements between lists *)
(* if any list is empty return empty list *)
