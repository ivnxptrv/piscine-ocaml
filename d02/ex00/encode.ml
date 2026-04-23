(* eval returns a function *)

type 'a run_length = 'a list -> (int * 'a) list

(* type run_length = char list -> (int * char) list *)

let encode lst =
  let rec next lst symbol count =
    match lst with
    | [] ->
        [(count, symbol)]
    | head :: tail ->
        if head = symbol then next tail symbol (count + 1)
        else (count, symbol) :: next tail head 1
  in
  match lst with [] -> [] | head :: tail -> next tail head 1

(* we eval right to get matched to () *)
let () =
  let rec print encoded_lst =
    match encoded_lst with
    | [] ->
        print_char '\n'
    | (count, symbol) :: tail ->
        print_int count ; print_char symbol ; print tail
  in
  let examples =
    [ ['a'; 'a'; 'a'; 'b'; 'b'; 'b'; 'c']
    ; ['a']
    ; ['a'; 'b'; 'c']
    ; ['x'; 'x'; 'y'; 'x'; 'x']
    ; [] ]
  in
  List.iter (fun example -> print (encode example)) examples
(* ocamlopt *.ml && ./a.out *)
(* aaabbb -> 3a3b *)
(* let lst = [ 'a'; 'a'; 'a'; 'b'; 'b'; 'b'; 'c' ] *)
(* let encoded_lst = [ (3, 'a'); (3, 'b'); (1, 'c') ]  *)
