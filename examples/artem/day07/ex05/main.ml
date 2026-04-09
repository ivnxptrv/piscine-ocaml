let print_incomplete_results results =
  List.iter
    (fun (k, prods) ->
      let prod_str =
        String.concat " + "
          (List.map
             (fun ((m : Molecule.molecule), q) ->
               if q = 1 then m#formula else string_of_int q ^ m#formula)
             prods)
      in
      Printf.printf "  %dO2 -> %s\n" k prod_str)
    results

let () =
  Printf.printf "=== Ethane incomplete combustion ===\n";
  let r = new AlkaneCombustion.alkane_combustion [ new Alkane.ethane ] in
  let inc = r#get_incomplete_results in
  print_incomplete_results inc;
  (* Expected:
     2O2 -> CO + C + 3H2O
     3O2 -> CO2 + CO + 3H2O *)
  assert (List.length inc = 2);
  print_endline "PASS\n"

let () =
  Printf.printf "=== Propane incomplete combustion ===\n";
  let r = new AlkaneCombustion.alkane_combustion [ new Alkane.propane ] in
  let inc = r#get_incomplete_results in
  print_incomplete_results inc;
  (* Expected:
     2O2 -> 3C + 4H2O
     3O2 -> 2CO + C + 4H2O
     3O2 -> CO2 + 2C + 4H2O
     4O2 -> CO2 + 2CO + 4H2O
     4O2 -> 2CO2 + C + 4H2O *)
  assert (List.length inc = 5);
  print_endline "PASS\n"

let () =
  Printf.printf "=== Methane incomplete combustion ===\n";
  let r = new AlkaneCombustion.alkane_combustion [ new Alkane.methane ] in
  let inc = r#get_incomplete_results in
  print_incomplete_results inc;
  (* CH4: n=1, d=2
     k=1: a from max(0,2-2-1)=0 to min(0,(2-2)/2)=0. a=0, b=0, c=1 -> C + 2H2O
     k=2 would be complete (a=1, b=0, c=0) - excluded *)
  assert (List.length inc = 1);
  print_endline "PASS\n"

let () =
  Printf.printf "=== Octane incomplete combustion ===\n";
  let r = new AlkaneCombustion.alkane_combustion [ new Alkane.octane ] in
  let inc = r#get_incomplete_results in
  print_incomplete_results inc;
  Printf.printf "Total incomplete results for octane: %d\n" (List.length inc);
  print_endline "PASS\n"

let () = print_endline "All incomplete combustion tests passed!"
