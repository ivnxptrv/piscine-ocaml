let sequence n =
  let encode (lst : int list) : int list =
    let rec encode_aux (lst : int list) (curr : int) (n : int) : int list =
      match lst with
      | [] -> [ n; curr ]
      | h :: t ->
          if h = curr then encode_aux t h (n + 1)
          else n :: curr :: encode_aux t h 1
    in
    match lst with [] -> [] | h :: t -> encode_aux t h 1
  in
  let rec sequence_aux (n : int) : int list =
    if n <= 0 then []
    else if n = 1 then [ 1 ]
    else encode (sequence_aux (n - 1))
  in
  let digit_to_str (n : int) : string =
    match n with
    | 0 -> "0"
    | 1 -> "1"
    | 2 -> "2"
    | 3 -> "3"
    | 4 -> "4"
    | 5 -> "5"
    | 6 -> "6"
    | 7 -> "7"
    | 8 -> "8"
    | 9 -> "9"
    | _ -> raise (Invalid_argument "digit_to_str")
  in
  let rec sequence_to_string (lst : int list) : string =
    match lst with [] -> "" | h :: t -> digit_to_str h ^ sequence_to_string t
  in
  sequence_to_string (sequence_aux n)

(* Tests *)
let () =
  print_endline (sequence 1);
  print_endline (sequence 2);
  print_endline (sequence 3);
  print_endline (sequence 4);
  print_endline (sequence 5);
  print_endline (sequence 6);
  print_endline (sequence 7);
  print_endline (sequence 8);
  print_endline (sequence 9);
  print_endline (sequence 10)
