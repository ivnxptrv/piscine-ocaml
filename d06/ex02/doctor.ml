class doctor name age sidekick =
  object (self)
    val _name : string = name

    val mutable _age : int = age

    val _sidekick : People.people = sidekick

    val mutable _hp : int = 100

    method to_string : string =
      Printf.sprintf "doctor(name=%s, age=%d, hp=%d, sidekick=%s)" _name _age
        _hp _sidekick#to_string

    method talk : unit = Printf.printf "Hi! I'm the Doctor!\n"

    initializer
      Printf.printf "Initializing a doctor %s %d years old with %d HP\n" _name
        _age _hp

    method travel_in_time start arrival =
      _age <- _age + abs (arrival - start) ;
      print_endline "          ___" ;
      print_endline "  _______(_@_)_______" ;
      print_endline " | POLICE      BOX   |" ;
      print_endline " |___________________|" ;
      print_endline "  | _ | | == | | _ |" ;
      print_endline "  | _ | | == | | _ |" ;
      print_endline "  |   | |    | |   |" ;
      print_endline "  |   | |    | |   |" ;
      print_endline "  |   | |    | |   |" ;
      print_endline "  |   | |    | |   |" ;
      print_endline "  |   | |    | |   |" ;
      print_endline "  |   | |    | |   |" ;
      print_endline "  |___|_|____|_|___|"

    method use_sonic_screwdriver =
      print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"

    method private regenerate =
      print_endline "Regenerating..." ;
      _hp <- 100

    (* to be able to test regenerate *)
    method take_damage (amount : int) =
      _hp <- _hp - amount ;
      Printf.printf "Doctor %s takes %d damage! HP: %d\n" _name amount _hp ;
      if _hp <= 0 then self#regenerate
  end
