let print_reaction (r : Reaction.reaction) =
  let fmt_side side =
    String.concat " + "
      (List.map
         (fun ((m : Molecule.molecule), q) ->
           if q = 1 then m#formula else string_of_int q ^ m#formula)
         side)
  in
  Printf.printf "%s -> %s\n" (fmt_side r#get_start) (fmt_side r#get_result)

(* Helper: extract coefficient list from a reaction *)
let coeffs_of (r : Reaction.reaction) =
  let s = List.map snd r#get_start in
  let e = List.map snd r#get_result in
  (s, e)

(* Test 1: Methane combustion — CH4 + 2O2 -> CO2 + 2H2O *)
let () =
  Printf.printf "=== Test 1: Methane combustion ===\n";
  let r = new AlkaneCombustion.alkane_combustion [ new Alkane.methane ] in
  Printf.printf "is_balanced (before balance): %b (expected: false)\n"
    r#is_balanced;
  assert (not r#is_balanced);
  (try
     let _ = r#get_start in
     assert false
   with Reaction.Unbalanced ->
     Printf.printf "get_start raises Unbalanced: OK\n");
  let b = r#balance in
  Printf.printf "is_balanced (after balance): %b (expected: true)\n"
    b#is_balanced;
  assert b#is_balanced;
  print_string "Balanced: ";
  print_reaction b;
  let start_c, result_c = coeffs_of b in
  (* CH4:1, O2:2 -> CO2:1, H2O:2 *)
  assert (start_c = [ 1; 2 ]);
  assert (result_c = [ 1; 2 ]);
  print_endline "PASS\n"

(* Test 2: Ethane combustion — 2C2H6 + 7O2 -> 4CO2 + 6H2O *)
let () =
  Printf.printf "=== Test 2: Ethane combustion ===\n";
  let r = new AlkaneCombustion.alkane_combustion [ new Alkane.ethane ] in
  assert (not r#is_balanced);
  let b = r#balance in
  assert b#is_balanced;
  print_string "Balanced: ";
  print_reaction b;
  let start_c, result_c = coeffs_of b in
  (* 2C2H6 + 7O2 -> 4CO2 + 6H2O *)
  assert (start_c = [ 2; 7 ]);
  assert (result_c = [ 4; 6 ]);
  print_endline "PASS\n"

(* Test 3: Octane combustion — 2C8H18 + 25O2 -> 16CO2 + 18H2O *)
let () =
  Printf.printf "=== Test 3: Octane combustion ===\n";
  let r = new AlkaneCombustion.alkane_combustion [ new Alkane.octane ] in
  assert (not r#is_balanced);
  let b = r#balance in
  assert b#is_balanced;
  print_string "Balanced: ";
  print_reaction b;
  let start_c, result_c = coeffs_of b in
  (* 2C8H18 + 25O2 -> 16CO2 + 18H2O *)
  assert (start_c = [ 2; 25 ]);
  assert (result_c = [ 16; 18 ]);
  print_endline "PASS\n"

(* Test 4: Duplicate alkanes — two methanes should be deduplicated *)
let () =
  Printf.printf "=== Test 4: Duplicate methanes (dedup) ===\n";
  let r =
    new AlkaneCombustion.alkane_combustion
      [ new Alkane.methane; new Alkane.methane ]
  in
  let b = r#balance in
  assert b#is_balanced;
  print_string "Balanced: ";
  print_reaction b;
  (* 2 methanes deduped to qty=2: coeffs doubled then /GCD
     q=2, n=1: alkane=2*2=4, O2=2*(3+1)=8, CO2=2*2*1=4, H2O=2*2*2=8
     GCD(4,8,4,8)=4 -> 1,2,1,2 — same as single methane *)
  let start_c, result_c = coeffs_of b in
  assert (start_c = [ 1; 2 ]);
  assert (result_c = [ 1; 2 ]);
  print_endline "PASS\n"

(* Test 5: Mixed alkanes — methane + ethane *)
let () =
  Printf.printf "=== Test 5: Methane + Ethane mixed combustion ===\n";
  let r =
    new AlkaneCombustion.alkane_combustion
      [ new Alkane.methane; new Alkane.ethane ]
  in
  assert (not r#is_balanced);
  let b = r#balance in
  assert b#is_balanced;
  print_string "Balanced: ";
  print_reaction b;
  (* methane q=1,n=1: alkane=2, O2=4, CO2=2, H2O=4
     ethane  q=1,n=2: alkane=2, O2=7, CO2=4, H2O=6
     totals: CH4:2, C2H6:2, O2:11, CO2:6, H2O:10
     GCD(2,2,11,6,10) = 1 *)
  let start_c, result_c = coeffs_of b in
  assert (start_c = [ 2; 2; 11 ]);
  assert (result_c = [ 6; 10 ]);
  print_endline "PASS\n"

(* Test 6: balance of balance is still balanced *)
let () =
  Printf.printf "=== Test 6: balance().balance() idempotent ===\n";
  let r = new AlkaneCombustion.alkane_combustion [ new Alkane.methane ] in
  let b = r#balance in
  let b2 = b#balance in
  assert b2#is_balanced;
  let s1, r1 = coeffs_of b in
  let s2, r2 = coeffs_of b2 in
  assert (s1 = s2);
  assert (r1 = r2);
  print_endline "PASS\n"

(* Test 7: formulas in balanced reaction *)
let () =
  Printf.printf
    "=== Test 7: Verify molecule formulas in balanced reaction ===\n";
  let r = new AlkaneCombustion.alkane_combustion [ new Alkane.octane ] in
  let b = r#balance in
  let start_formulas =
    List.map (fun ((m : Molecule.molecule), _) -> m#formula) b#get_start
  in
  let result_formulas =
    List.map (fun ((m : Molecule.molecule), _) -> m#formula) b#get_result
  in
  Printf.printf "Start: %s\n" (String.concat ", " start_formulas);
  Printf.printf "Result: %s\n" (String.concat ", " result_formulas);
  assert (
    List.nth start_formulas 0 = "CH18" || List.nth start_formulas 0 = "C8H18");
  assert (List.nth start_formulas 1 = "O2");
  assert (List.nth result_formulas 0 = "CO2");
  assert (List.nth result_formulas 1 = "H2O");
  print_endline "PASS\n"

let () = print_endline "All alkane_combustion tests passed!"
