(* ocamlopt *.ml && ./a.out *)
(* Gray code (also known as Reflected Binary Code) is a binary numeral system *)
(*  where two successive values differ in only one bit position. *)
(* Gray code is designed so that "ghost states" are physically impossible *)
(*  because only one bit ever changes at a time. *)
(* In Gray code, the binary pattern for a number is different from its standard binary representation *)
(* stdbin - 3 = 11 / graybin = 3 = 10 *)
(* in the context of Gray code, that bit pattern specifically represents the third step in the sequence *)
(* all possible states nested *)
(* n -- number of bits *)
(* we pefix previous sequnece with 0 the mirror previous gray and prefix with 1 *)

(* let () = *)
(*   let lst1 = ["a"; "b"] in *)
(*   print_list (prepend_list lst1 "a") *)

(* let () = *)
(*   let lst1 = ["a"; "b"] in *)
(*   print_list (prepend_list lst1 "a") *)

let rec gray n =
  let rec reflect_list ?(acc = []) lst =
    match lst with
    | [] ->
        acc
    | head :: tail ->
        reflect_list ~acc:(head :: acc) tail
  in
  (* generates new list of strings with prepended string *)
  let rec prepend_list lst sym =
    match lst with
    | [] ->
        []
    | head :: tail ->
        (sym ^ head) :: prepend_list tail sym
  in
  (* we just reconcat lst1 and when we touch last elm of lst1 we concat :: with lst2 as its tail *)
  let rec concat_lists lst1 lst2 =
    match lst1 with
    | [] ->
        lst2
    | head :: tail ->
        head :: concat_lists tail lst2
  in
  (* main func body *)
  if n = 0 then [""]
  else
    let direct_seq = gray (n - 1) in
    let mirrored_seq = reflect_list direct_seq in
    let seq1 = prepend_list direct_seq "0" in
    let seq2 = prepend_list mirrored_seq "1" in
    concat_lists seq1 seq2

let () =
  let rec print_list = function
    | [] ->
        ()
    | head :: tail ->
        print_string head ; print_char ' ' ; print_list tail
  in
  print_list (gray 0) ;
  print_char '\n' ;
  print_list (gray 1) ;
  print_char '\n' ;
  print_list (gray 2) ;
  print_char '\n' ;
  print_list (gray 3) ;
  print_char '\n' ;
  print_list (gray 4) ;
  print_char '\n'

(* gray 0 = "" *)
(* gray 1 = "0 1" *)
(* gray 2 = "00 01 11 10" *)

(* 1. take previous seq *)
(* 2. make seq1 prepending previous_sequence with 0 *)
(* 3. mirror previos-squence *)
(* 4. make seq2 prepending mirrored_prvious_sequence with 1 *)
(* 5. concat seq1 and seq2 to resulting seq *)
