let () =
  let person = new People.people "John" in
  print_endline person#to_string;
  person#talk;
  person#die;
  print_endline person#to_string
