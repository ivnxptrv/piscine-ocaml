let () =
  let doctor =
    new Doctor.doctor "Tenth" 50 (new People.people "Martha Jones")
  in
  print_endline doctor#to_string;
  doctor#travel_in_time 2026 2007;
  print_endline doctor#to_string;
  doctor#use_sonic_screwdriver;
  doctor#take_damage 20;
  print_endline doctor#to_string;
  doctor#take_damage 80;
  print_endline doctor#to_string
