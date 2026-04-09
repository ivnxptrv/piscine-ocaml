let () =
  (* return - creates singleton *)
  let s = Set.return 42 in
  assert (s = [ 42 ]);
  print_endline "return: OK";

  (* bind - apply function to each element, changing type *)
  let s =
    Set.bind [ 1; 2; 3 ] (fun x -> [ string_of_int x; string_of_int (x * 10) ])
  in
  assert (List.length s = 6);
  assert (List.mem "1" s);
  assert (List.mem "10" s);
  assert (List.mem "2" s);
  assert (List.mem "20" s);
  assert (List.mem "3" s);
  assert (List.mem "30" s);
  print_endline "bind changes type: OK";

  (* bind - duplicates are removed *)
  let s = Set.bind [ 1; 2; 3 ] (fun _ -> Set.return 0) in
  assert (s = [ 0 ]);
  print_endline "bind dedup: OK";

  (* bind on empty set *)
  let result = Set.bind [] (fun x -> Set.return (x + 1)) in
  assert (result = []);
  print_endline "bind empty: OK";

  (* monad left identity: bind (return x) f = f x *)
  let f x = [ x; x + 1; x + 2 ] in
  let lhs = Set.bind (Set.return 5) f in
  let rhs = List.sort_uniq compare (f 5) in
  assert (lhs = rhs);
  print_endline "monad left identity: OK";

  (* monad right identity: bind s return = s *)
  let s = List.sort_uniq compare [ 3; 1; 2 ] in
  let result = Set.bind s Set.return in
  assert (result = s);
  print_endline "monad right identity: OK";

  (* union *)
  let s = Set.union [ 1; 2; 3 ] [ 3; 4; 5 ] in
  assert (List.length s = 5);
  assert (List.mem 1 s && List.mem 2 s && List.mem 3 s);
  assert (List.mem 4 s && List.mem 5 s);
  print_endline "union: OK";

  (* union - dedup *)
  let s = Set.union [ 1; 2 ] [ 1; 2 ] in
  assert (s = [ 1; 2 ]);
  print_endline "union dedup: OK";

  (* inter *)
  let s = Set.inter [ 1; 2; 3; 4 ] [ 2; 4; 6 ] in
  assert (s = [ 2; 4 ]);
  print_endline "inter: OK";

  (* inter - no overlap *)
  let s = Set.inter [ 1; 2 ] [ 3; 4 ] in
  assert (s = []);
  print_endline "inter no overlap: OK";

  (* diff *)
  let s = Set.diff [ 1; 2; 3; 4 ] [ 2; 4 ] in
  assert (s = [ 1; 3 ]);
  print_endline "diff: OK";

  (* diff - empty result *)
  let s = Set.diff [ 1; 2 ] [ 1; 2; 3 ] in
  assert (s = []);
  print_endline "diff empty result: OK";

  (* filter *)
  let s = Set.filter [ 1; 2; 3; 4; 5 ] (fun x -> x mod 2 = 0) in
  assert (s = [ 2; 4 ]);
  print_endline "filter: OK";

  (* filter - none match *)
  let s = Set.filter [ 1; 3; 5 ] (fun x -> x mod 2 = 0) in
  assert (s = []);
  print_endline "filter none match: OK";

  (* foreach *)
  let sum = ref 0 in
  Set.foreach [ 1; 2; 3 ] (fun x -> sum := !sum + x);
  assert (!sum = 6);
  print_endline "foreach: OK";

  (* for_all *)
  assert (Set.for_all [ 2; 4; 6 ] (fun x -> x mod 2 = 0));
  assert (not (Set.for_all [ 2; 3; 6 ] (fun x -> x mod 2 = 0)));
  print_endline "for_all: OK";

  (* exists *)
  assert (Set.exists [ 1; 3; 4 ] (fun x -> x mod 2 = 0));
  assert (not (Set.exists [ 1; 3; 5 ] (fun x -> x mod 2 = 0)));
  print_endline "exists: OK";

  print_endline "All Set tests passed!"
