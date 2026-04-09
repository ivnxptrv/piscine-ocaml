let doctor_attacks_dalek doctor dalek =
  print_endline "The Doctor uses the sonic screwdriver!";
  doctor#use_sonic_screwdriver;
  dalek#take_damage 25

let dalek_attacks target =
  print_endline "Exterminate!";
  target#take_damage 30

let () =
  print_endline "=== BATTLE SIMULATION: Doctor vs Dalek vs Human ===\n";

  (* Create characters *)
  print_endline "--- Creating characters ---";
  let companion = new People.people "Rose Tyler" in
  let doctor = new Doctor.doctor "Who" 900 companion in
  let dalek = new Dalek.dalek in
  print_newline ();

  (* Initial status *)
  print_endline "--- Initial Status ---";
  print_endline companion#to_string;
  print_endline doctor#to_string;
  print_endline dalek#to_string;
  print_newline ();

  (* Characters introduce themselves *)
  print_endline "--- Introductions ---";
  companion#talk;
  doctor#talk;
  print_newline ();
  dalek#talk;
  print_newline ();

  (* Round 1: Dalek attacks the Doctor *)
  print_endline "--- Round 1: Dalek attacks the Doctor ---";
  dalek_attacks doctor;
  print_endline doctor#to_string;
  print_newline ();

  (* Round 2: Doctor fights back *)
  print_endline "--- Round 2: Doctor counterattacks ---";
  doctor_attacks_dalek doctor dalek;
  print_endline dalek#to_string;
  print_newline ();

  (* Round 3: Dalek attacks again *)
  print_endline "--- Round 3: Dalek attacks again ---";
  dalek_attacks doctor;
  print_endline doctor#to_string;
  print_newline ();

  (* Round 4: Doctor attacks - shield should be down now *)
  print_endline "--- Round 4: Doctor attacks (shield down) ---";
  doctor_attacks_dalek doctor dalek;
  print_endline dalek#to_string;
  print_newline ();

  (* Round 5: Dalek goes after the companion *)
  print_endline "--- Round 5: Dalek targets companion ---";
  dalek#exterminate companion;
  print_endline companion#to_string;
  print_endline dalek#to_string;
  print_newline ();

  (* Round 6: Doctor retaliates heavily *)
  print_endline "--- Round 6: Doctor retaliates ---";
  doctor_attacks_dalek doctor dalek;
  doctor_attacks_dalek doctor dalek;
  print_endline dalek#to_string;
  print_newline ();

  (* The Doctor uses time travel *)
  print_endline "--- The Doctor travels back in time to save Rose ---";
  doctor#travel_in_time 2024 2020;
  print_endline doctor#to_string;
  print_newline ();

  (* Dalek repairs itself *)
  print_endline "--- Dalek repairs itself ---";
  dalek#repair;
  print_endline dalek#to_string;
  print_newline ();

  (* Final showdown: Doctor takes massive damage and regenerates *)
  print_endline "--- Final Showdown ---";
  dalek_attacks doctor;
  dalek_attacks doctor;
  dalek_attacks doctor;
  dalek_attacks doctor;
  print_endline doctor#to_string;
  print_newline ();

  (* Doctor defeats the Dalek *)
  print_endline "--- Doctor delivers final blow ---";
  doctor_attacks_dalek doctor dalek;
  doctor_attacks_dalek doctor dalek;
  doctor_attacks_dalek doctor dalek;
  doctor_attacks_dalek doctor dalek;
  doctor_attacks_dalek doctor dalek;
  print_newline ();

  (* Final status *)
  print_endline "=== BATTLE COMPLETE ===";
  print_endline "--- Final Status ---";
  print_endline companion#to_string;
  print_endline doctor#to_string;
  print_endline dalek#to_string
