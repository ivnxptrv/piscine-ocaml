let () =
  print_endline "=== PREPARING FOR THE TIME WAR ===\n";

  let gallifrey = new Galifrey.galifrey in

  (* Create the defenders of Gallifrey *)
  print_endline "--- Assembling the Doctors ---";
  let companion1 = new People.people "Rose Tyler" in
  let companion2 = new People.people "Clara Oswald" in
  let companion3 = new People.people "Donna Noble" in

  let doctor1 = new Doctor.doctor "War" 800 companion1 in
  let doctor2 = new Doctor.doctor "Ten" 903 companion2 in
  let doctor3 = new Doctor.doctor "Eleven" 1200 companion3 in

  gallifrey#add_doctor doctor1;
  gallifrey#add_doctor doctor2;
  gallifrey#add_doctor doctor3;
  print_newline ();

  (* Create the Time Lords (people) *)
  print_endline "--- Assembling the Time Lords ---";
  let timelord1 = new People.people "Rassilon" in
  let timelord2 = new People.people "Romana" in
  let timelord3 = new People.people "The Master" in

  gallifrey#add_people timelord1;
  gallifrey#add_people timelord2;
  gallifrey#add_people timelord3;
  print_newline ();

  (* Create the Dalek invasion force *)
  print_endline "--- The Dalek Fleet Arrives ---";
  let rec create_daleks n =
    if n <= 0 then ()
    else
      let dalek = new Dalek.dalek in
      gallifrey#add_dalek dalek;
      create_daleks (n - 1)
  in
  create_daleks 5;
  print_newline ();

  (* Begin the Time War *)
  gallifrey#do_time_war
