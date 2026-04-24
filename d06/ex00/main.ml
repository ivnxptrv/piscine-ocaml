let () =
  let person = new People.people "John" in
  print_endline person#to_string ;
  ignore person#talk ;
  ignore person#die ;
  print_endline person#to_string
