let () =
  let person = new People.people "John" in
  let doctor = new Doctor.doctor "Tenth" 50 person in
  print_endline doctor#to_string ;
  doctor#talk ;
  doctor#travel_in_time 2042 2026 ;
  (* show what changed after time travel *)
  print_endline doctor#to_string ;
  doctor#use_sonic_screwdriver ;
  doctor#take_damage 5 ;
  (* show what changed after time travel (regenerated back or not)*)
  print_endline doctor#to_string ;
  doctor#take_damage 500 ;
  (* show what changed after time travel (regenerated back or not)*)
  print_endline doctor#to_string
(* doctor#regenerate *)
