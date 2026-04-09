let rng = Random.State.make_self_init ()

let pick_random lst =
  let len = List.length lst in
  if len = 0 then None
  else
    let idx = Random.State.int rng len in
    Some (List.nth lst idx)

class galifrey =
  object (self)
    val mutable _daleks : Dalek.dalek list = []
    val mutable _doctors : Doctor.doctor list = []
    val mutable _people : People.people list = []
    method add_dalek (d : Dalek.dalek) = _daleks <- d :: _daleks
    method add_doctor (d : Doctor.doctor) = _doctors <- d :: _doctors
    method add_people (p : People.people) = _people <- p :: _people
    method get_daleks = _daleks
    method get_doctors = _doctors
    method get_people = _people
    method private daleks_alive = List.filter (fun d -> d#is_alive) _daleks
    method private doctors_alive = List.filter (fun d -> d#is_alive) _doctors
    method private people_alive = List.filter (fun p -> p#is_alive) _people
    method private count_alive_daleks = List.length self#daleks_alive
    method private count_alive_doctors = List.length self#doctors_alive
    method private count_alive_people = List.length self#people_alive

    method private dalek_turn (dalek : Dalek.dalek) =
      if dalek#is_alive then
        let doctors = self#doctors_alive in
        let people = self#people_alive in
        match (doctors, people) with
        | [], [] -> ()
        | _ :: _, _ -> (
            match pick_random doctors with
            | Some doc -> dalek#attack (doc :> < take_damage : int -> unit >)
            | None -> ())
        | [], _ :: _ -> (
            match pick_random people with
            | Some person ->
                dalek#attack (person :> < take_damage : int -> unit >)
            | None -> ())

    method private doctor_turn (doctor : Doctor.doctor) =
      if doctor#is_alive then
        let daleks = self#daleks_alive in
        match pick_random daleks with
        | Some dalek -> doctor#attack (dalek :> < take_damage : int -> unit >)
        | None -> ()

    method private print_status () =
      print_endline "\n--- Current Status ---";
      Printf.printf "Daleks alive: %d\n" self#count_alive_daleks;
      List.iter
        (fun d -> if d#is_alive then print_endline ("  " ^ d#to_string))
        _daleks;
      Printf.printf "Doctors alive: %d\n" self#count_alive_doctors;
      List.iter
        (fun d -> if d#is_alive then print_endline ("  " ^ d#to_string))
        _doctors;
      Printf.printf "People alive: %d\n" self#count_alive_people;
      List.iter
        (fun p -> if p#is_alive then print_endline ("  " ^ p#to_string))
        _people;
      print_newline ()

    method private war_continues =
      self#count_alive_daleks > 0
      && (self#count_alive_doctors > 0 || self#count_alive_people > 0)

    method private do_round round_num =
      if self#war_continues then (
        Printf.printf "\n========== ROUND %d ==========\n" round_num;

        print_endline "\n-- Daleks Attack --";
        List.iter self#dalek_turn self#daleks_alive;

        print_endline "\n-- Doctors Counterattack --";
        List.iter self#doctor_turn self#doctors_alive;

        self#print_status ();

        self#do_round (round_num + 1))

    method do_time_war =
      print_endline "========================================";
      print_endline "        THE TIME WAR BEGINS!            ";
      print_endline "========================================";

      self#print_status ();

      self#do_round 1;

      print_endline "========================================";
      print_endline "        THE TIME WAR HAS ENDED!         ";
      print_endline "========================================";

      if self#count_alive_daleks = 0 then
        print_endline "The Doctors have won! Gallifrey stands!"
      else if self#count_alive_doctors = 0 && self#count_alive_people = 0 then
        print_endline "The Daleks have won! Gallifrey has fallen!"
      else print_endline "The war continues..."
  end
