let () =
  let atoms =
    [
      new Atom.hydrogen;
      new Atom.carbon;
      new Atom.oxygen;
      new Atom.nitrogen;
      new Atom.helium;
      new Atom.iron;
    ]
  in

  print_endline "--- Testing to_string ---";
  List.iter (fun a -> print_endline a#to_string) atoms;

  print_endline "\n--- Testing accessors ---";
  List.iter
    (fun a ->
      Printf.printf "name=%s, symbol=%s, atomic_number=%d\n" a#name a#symbol
        a#atomic_number)
    atoms;

  print_endline "\n--- Testing equals ---";
  let h1 = List.hd atoms in
  let h2 = new Atom.hydrogen in
  let c = List.nth atoms 1 in
  Printf.printf "hydrogen equals hydrogen: %b\n" (h1#equals h2);
  Printf.printf "hydrogen equals carbon: %b\n" (h1#equals c)
