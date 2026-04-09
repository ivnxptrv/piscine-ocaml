let () =
  (* Test zero *)
  Printf.printf "=== zero ===\n";
  Printf.printf "zero = %d (expected 12): %s\n" Watchtower.zero
    (if Watchtower.zero = 12 then "OK" else "FAIL");

  (* Test add: basic cases *)
  Printf.printf "\n=== add ===\n";
  let test_add h1 h2 expected =
    let result = Watchtower.add h1 h2 in
    Printf.printf "add %d %d = %d (expected %d): %s\n" h1 h2 result expected
      (if result = expected then "OK" else "FAIL")
  in
  test_add 0 0 0;
  test_add 1 2 3;
  test_add 6 6 0;
  test_add 5 7 0;
  test_add 11 1 0;
  test_add 3 9 0;
  test_add 4 5 9;
  test_add 10 3 1;
  test_add 12 0 0;

  (* Test sub: basic cases *)
  Printf.printf "\n=== sub ===\n";
  let test_sub h1 h2 expected =
    let result = Watchtower.sub h1 h2 in
    Printf.printf "sub %d %d = %d (expected %d): %s\n" h1 h2 result expected
      (if result = expected then "OK" else "FAIL")
  in
  test_sub 0 0 0;
  test_sub 5 3 2;
  test_sub 10 4 6;
  test_sub 11 11 0;
  test_sub 3 5 10;
  test_sub 0 7 5;
  test_sub 12 12 0;
  test_sub 1 12 1
