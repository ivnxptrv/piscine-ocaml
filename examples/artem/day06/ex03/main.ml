let () =
  print_endline "=== ARMY CONSTRUCTION AND DESTRUCTION TEST ===\n";

  (* Test People Army *)
  print_endline "--- Building People Army ---";
  let people_army = new Army.army in
  let person1 = new People.people "Rose Tyler" in
  let person2 = new People.people "Martha Jones" in
  let person3 = new People.people "Donna Noble" in
  people_army#add person1;
  people_army#add person2;
  people_army#add person3;
  Printf.printf "People army size: %d\n" people_army#size;
  List.iter (fun p -> print_endline p#to_string) people_army#get_members;
  print_newline ();

  print_endline "--- Removing from People Army ---";
  people_army#delete;
  Printf.printf "People army size after delete: %d\n" people_army#size;
  List.iter (fun p -> print_endline p#to_string) people_army#get_members;
  print_newline ();

  (* Test Dalek Army *)
  print_endline "--- Building Dalek Army ---";
  let dalek_army = new Army.army in
  let dalek1 = new Dalek.dalek in
  let dalek2 = new Dalek.dalek in
  let dalek3 = new Dalek.dalek in
  let dalek4 = new Dalek.dalek in
  dalek_army#add dalek1;
  dalek_army#add dalek2;
  dalek_army#add dalek3;
  dalek_army#add dalek4;
  Printf.printf "Dalek army size: %d\n" dalek_army#size;
  List.iter (fun d -> print_endline d#to_string) dalek_army#get_members;
  print_newline ();

  print_endline "--- Destroying Dalek Army ---";
  dalek_army#delete;
  dalek_army#delete;
  Printf.printf "Dalek army size after 2 deletes: %d\n" dalek_army#size;
  List.iter (fun d -> print_endline d#to_string) dalek_army#get_members;
  print_newline ();

  (* Test Doctor Army *)
  print_endline "--- Building Doctor Army ---";
  let doctor_army = new Army.army in
  let companion1 = new People.people "Amy Pond" in
  let companion2 = new People.people "Clara Oswald" in
  let companion3 = new People.people "Bill Potts" in
  let doctor1 = new Doctor.doctor "Ten" 903 companion1 in
  let doctor2 = new Doctor.doctor "Eleven" 1200 companion2 in
  let doctor3 = new Doctor.doctor "Twelve" 2000 companion3 in
  doctor_army#add doctor1;
  doctor_army#add doctor2;
  doctor_army#add doctor3;
  Printf.printf "Doctor army size: %d\n" doctor_army#size;
  List.iter (fun d -> print_endline d#to_string) doctor_army#get_members;
  print_newline ();

  print_endline "--- Removing from Doctor Army ---";
  doctor_army#delete;
  Printf.printf "Doctor army size after delete: %d\n" doctor_army#size;
  List.iter (fun d -> print_endline d#to_string) doctor_army#get_members;
  print_newline ();

  (* Complete destruction test *)
  print_endline "--- Complete Destruction of People Army ---";
  people_army#delete;
  people_army#delete;
  people_army#delete;
  Printf.printf "People army size after complete destruction: %d\n"
    people_army#size;
  print_newline ();

  print_endline "=== ARMY TEST COMPLETE ==="
