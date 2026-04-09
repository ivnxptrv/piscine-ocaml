let encode lst =
  let rec encode_aux lst symbol count =
    match lst with
    | [] -> [ (count, symbol) ]
    | head :: tail ->
        if head = symbol then encode_aux tail symbol (count + 1)
        else (count, symbol) :: encode_aux tail head 1
  in
  match lst with [] -> [] | head :: tail -> encode_aux tail head 1

(* Tests *)
let rec print_encoded_list = function
  | [] -> print_newline ()
  | (count, symbol) :: tail ->
      print_int count;
      print_char symbol;
      print_encoded_list tail

let () =
  print_encoded_list
    (encode
       [ 'a'; 'a'; 'a'; 'a'; 'b'; 'c'; 'c'; 'a'; 'a'; 'd'; 'e'; 'e'; 'e'; 'e' ]);
  print_encoded_list (encode [])
