(* Concrete reaction for testing — balance returns self *)
class trivial_reaction start result =
  object (self)
    inherit Reaction.reaction start result
    method balance = (self :> Reaction.reaction)
  end

(* 2H2 + O2 -> 2H2O  (balanced) *)
let () =
  let h2o = new Molecule.water in
  let o2 = new Molecule.dioxygen in
  (* Build H2: 2 hydrogens *)
  let h2 =
    object
      inherit
        Molecule.molecule
          "dihydrogen"
          (List.init 2 (fun _ -> new Atom.hydrogen))
    end
  in
  let r = new trivial_reaction [ (h2, 2); (o2, 1) ] [ (h2o, 2) ] in
  Printf.printf "=== Test 1: 2H2 + O2 -> 2H2O ===\n";
  Printf.printf "is_balanced: %b (expected: true)\n" r#is_balanced;
  assert r#is_balanced;
  let start = r#get_start in
  let result = r#get_result in
  Printf.printf "start length: %d (expected: 2)\n" (List.length start);
  Printf.printf "result length: %d (expected: 1)\n" (List.length result);
  assert (List.length start = 2);
  assert (List.length result = 1);
  print_endline "PASS\n"

(* Unbalanced: H2 -> H2O *)
let () =
  let h2o = new Molecule.water in
  let h2 =
    object
      inherit
        Molecule.molecule
          "dihydrogen"
          (List.init 2 (fun _ -> new Atom.hydrogen))
    end
  in
  let r = new trivial_reaction [ (h2, 1) ] [ (h2o, 1) ] in
  Printf.printf "=== Test 2: H2 -> H2O (unbalanced) ===\n";
  Printf.printf "is_balanced: %b (expected: false)\n" r#is_balanced;
  assert (not r#is_balanced);
  (try
     let _ = r#get_start in
     assert false
   with Reaction.Unbalanced ->
     Printf.printf "get_start raised Unbalanced: OK\n");
  (try
     let _ = r#get_result in
     assert false
   with Reaction.Unbalanced ->
     Printf.printf "get_result raised Unbalanced: OK\n");
  print_endline "PASS\n"

(* Balanced: CH4 + 2O2 -> CO2 + 2H2O *)
let () =
  let ch4 = new Molecule.methane in
  let o2 = new Molecule.dioxygen in
  let co2 = new Molecule.carbon_dioxide in
  let h2o = new Molecule.water in
  let r = new trivial_reaction [ (ch4, 1); (o2, 2) ] [ (co2, 1); (h2o, 2) ] in
  Printf.printf "=== Test 3: CH4 + 2O2 -> CO2 + 2H2O ===\n";
  Printf.printf "is_balanced: %b (expected: true)\n" r#is_balanced;
  assert r#is_balanced;
  let start = r#get_start in
  let result = r#get_result in
  assert (List.length start = 2);
  assert (List.length result = 2);
  print_endline "PASS\n"

(* Unbalanced: CH4 + O2 -> CO2 + H2O (wrong coefficients) *)
let () =
  let ch4 = new Molecule.methane in
  let o2 = new Molecule.dioxygen in
  let co2 = new Molecule.carbon_dioxide in
  let h2o = new Molecule.water in
  let r = new trivial_reaction [ (ch4, 1); (o2, 1) ] [ (co2, 1); (h2o, 1) ] in
  Printf.printf "=== Test 4: CH4 + O2 -> CO2 + H2O (unbalanced) ===\n";
  Printf.printf "is_balanced: %b (expected: false)\n" r#is_balanced;
  assert (not r#is_balanced);
  print_endline "PASS\n"

(* Balanced with same molecule both sides: trivially empty *)
let () =
  let r = new trivial_reaction [] [] in
  Printf.printf "=== Test 5: empty reaction ===\n";
  Printf.printf "is_balanced: %b (expected: true)\n" r#is_balanced;
  assert r#is_balanced;
  assert (r#get_start = []);
  assert (r#get_result = []);
  print_endline "PASS\n"

(* Test balance method returns a reaction *)
let () =
  let h2o = new Molecule.water in
  let o2 = new Molecule.dioxygen in
  let h2 =
    object
      inherit
        Molecule.molecule
          "dihydrogen"
          (List.init 2 (fun _ -> new Atom.hydrogen))
    end
  in
  let r = new trivial_reaction [ (h2, 2); (o2, 1) ] [ (h2o, 2) ] in
  let balanced = r#balance in
  Printf.printf "=== Test 6: balance method ===\n";
  Printf.printf "balance#is_balanced: %b (expected: true)\n"
    balanced#is_balanced;
  assert balanced#is_balanced;
  print_endline "PASS\n"

(* Unbalanced: atoms on result side not present on start side *)
let () =
  let h2 =
    object
      inherit
        Molecule.molecule
          "dihydrogen"
          (List.init 2 (fun _ -> new Atom.hydrogen))
    end
  in
  let h2o = new Molecule.water in
  let r = new trivial_reaction [ (h2, 1) ] [ (h2o, 1) ] in
  Printf.printf "=== Test 7: extra atoms on result side ===\n";
  Printf.printf "is_balanced: %b (expected: false)\n" r#is_balanced;
  assert (not r#is_balanced);
  print_endline "PASS\n"

let () = print_endline "All Reaction tests passed!"
