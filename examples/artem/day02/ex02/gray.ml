let gray n =
  let rec prefix_strings_list lst prefix =
    match lst with
    | [] -> []
    | head :: tail -> (prefix ^ head) :: prefix_strings_list tail prefix
  in
  let rec reverse_list ?(acc = []) lst =
    match lst with
    | [] -> acc
    | head :: tail -> reverse_list ~acc:(head :: acc) tail
  in
  let rec concat_lists lst1 lst2 =
    match lst1 with [] -> lst2 | x :: xs -> x :: concat_lists xs lst2
  in
  let rec gen_gray n =
    if n = 0 then [ "" ]
    else
      let prev_gray = gen_gray (n - 1) in
      let prefix0 = prefix_strings_list prev_gray "0" in
      let prefix1 = prefix_strings_list (reverse_list prev_gray) "1" in
      concat_lists prefix0 prefix1
  in
  if n < 0 then [] else gen_gray n

(* Tests *)
let () =
  let test_cases =
    [
      (0, [ "" ]);
      (1, [ "0"; "1" ]);
      (2, [ "00"; "01"; "11"; "10" ]);
      (3, [ "000"; "001"; "011"; "010"; "110"; "111"; "101"; "100" ]);
      ( 4,
        [
          "0000";
          "0001";
          "0011";
          "0010";
          "0110";
          "0111";
          "0101";
          "0100";
          "1100";
          "1101";
          "1111";
          "1110";
          "1010";
          "1011";
          "1001";
          "1000";
        ] );
    ]
  in
  List.iter
    (fun (n, expected) ->
      let result = gray n in
      List.iter (fun s -> Printf.printf "%s " s) result;
      Printf.printf "\n";
      assert (result = expected);
      Printf.printf "Test passed for n = %d\n" n)
    test_cases
