let () =
  (* If you don't call self_init, the sequence always starts from the same default seed (usually 0). *)
  Random.self_init () ;
  let jokes =
    [| "What's red and goes up and down? A tomato in an elevator."
     ; "What do you call a fake noodle? An impasta."
     ; "Why did the scarecrow win an award? Because he was outstanding in his \
        field."
     ; "What do you call a belt made out of watches? A waist of time."
     ; "Why don't skeletons fight each other? They don't have the guts." |]
  in
  (* Random.int n returns a value from 0 to n-1 *)
  let index = Random.int (Array.length jokes) in
  print_endline jokes.(index)
(* jokes.(index) <- "hello" ; *)
(* print_endline jokes.(index) *)
