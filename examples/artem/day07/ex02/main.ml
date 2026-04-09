let () =
  let methane = new Alkane.methane in
  let ethane = new Alkane.ethane in
  let octane = new Alkane.octane in
  print_endline "--- name ---";
  methane#name |> print_endline;
  ethane#name |> print_endline;
  octane#name |> print_endline;
  print_endline "--- to_string ---";
  methane#to_string |> print_endline;
  ethane#to_string |> print_endline;
  octane#to_string |> print_endline;
  print_endline "--- formula ---";
  methane#formula |> print_endline;
  ethane#formula |> print_endline;
  octane#formula |> print_endline;
  print_endline "--- equals ---";
  (* Test self-equality *)
  print_endline
    ("methane equals methane: " ^ string_of_bool (methane#equals methane));
  print_endline
    ("ethane equals ethane: " ^ string_of_bool (ethane#equals ethane));
  print_endline
    ("octane equals octane: " ^ string_of_bool (octane#equals octane));
  (* Test different instances of same type *)
  let methane2 = new Alkane.methane in
  let ethane2 = new Alkane.ethane in
  print_endline
    ("methane equals methane2: " ^ string_of_bool (methane#equals methane2));
  print_endline
    ("ethane equals ethane2: " ^ string_of_bool (ethane#equals ethane2));
  (* Test different molecules *)
  print_endline
    ("methane equals ethane: " ^ string_of_bool (methane#equals ethane));
  print_endline
    ("ethane equals octane: " ^ string_of_bool (ethane#equals octane));
  print_endline
    ("octane equals methane: " ^ string_of_bool (octane#equals methane))
