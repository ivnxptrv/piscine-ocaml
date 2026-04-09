let () =
  print_endline "--- Testing molecule creation and to_string ---";
  let w = new Molecule.water in
  let co2 = new Molecule.carbon_dioxide in
  let ch4 = new Molecule.methane in
  let nh3 = new Molecule.ammonia in
  let eth = new Molecule.ethanol in

  w#to_string |> print_endline;
  co2#to_string |> print_endline;
  ch4#to_string |> print_endline;
  nh3#to_string |> print_endline;
  eth#to_string |> print_endline;

  print_endline "\n--- Testing equals method ---";
  let w2 = new Molecule.water in
  Printf.printf "water equals water: %b\n" (w#equals (w2 :> Molecule.molecule));
  Printf.printf "water equals methane: %b\n"
    (w#equals (ch4 :> Molecule.molecule));
  Printf.printf "ethanol equals ethanol: %b\n"
    (eth#equals (eth :> Molecule.molecule))
