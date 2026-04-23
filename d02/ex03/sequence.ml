(* ocamlopt *.ml && ./a.out *)

(* 1. start from list of ints [ 1 ] *)
(* 2. encode previous list -> encoded list -> [ 1; 1 ] *)
(* 3. use encoded list as new input *)
(* 4. repeat from step 1 n times *)
(* 5. convert list of ints to string and return *)

(* sequence starts with 1; on next step encode(by length) rules applied *)
(* reurn nth element of a sequence *)
(* int -> string *)
let sequence n =
  let encode lst =
    let rec next lst symbol count =
      match lst with
      | [] ->
          [count; symbol]
      | head :: tail ->
          if head = symbol then next tail symbol (count + 1)
          else count :: symbol :: next tail head 1
    in
    match lst with [] -> [] | head :: tail -> next tail head 1
  in
  let rec int_lst_to_string lst =
    let digit_to_str = function
      | 0 ->
          "0"
      | 1 ->
          "1"
      | 2 ->
          "2"
      | 3 ->
          "3"
      | 4 ->
          "4"
      | 5 ->
          "5"
      | 6 ->
          "6"
      | 7 ->
          "7"
      | 8 ->
          "8"
      | 9 ->
          "9"
      | _ ->
          raise (Invalid_argument "digit_to_str")
    in
    match lst with
    | [] ->
        ""
    | head :: tail ->
        digit_to_str head ^ int_lst_to_string tail
  in
  let acc = [1] in
  let rec calc n acc = if n = 1 then acc else calc (n - 1) (encode acc) in
  if n <= 0 then "" else int_lst_to_string (calc n acc)

let () =
  print_endline (sequence 1) ;
  print_endline (sequence 2) ;
  print_endline (sequence 3) ;
  print_endline (sequence 4) ;
  print_endline (sequence 5) ;
  print_endline (sequence 6) ;
  print_endline (sequence 7) ;
  print_endline (sequence 8) ;
  print_endline (sequence 9) ;
  print_endline (sequence 10) ;
  print_endline (sequence 0) ;
  print_endline (sequence (-1))
