class doctor name age (sidekick : People.people) =
  object (self)
    val _name = name
    val mutable _age = age
    val mutable _hp = 100
    val _sidekick = sidekick
    initializer Printf.printf "Doctor %s is born!\n" _name

    method to_string =
      Printf.sprintf "doctor(name=%s, age=%d, hp=%d, sidekick=%s)" _name _age
        _hp _sidekick#to_string

    method talk = print_string "Hi! I'm the Doctor!"

    method private _print_tardis () =
      print_endline "          ___";
      print_endline "  _______(_@_)_______";
      print_endline " | POLICE      BOX   |";
      print_endline " |___________________|";
      print_endline "  | _ | | == | | _ |";
      print_endline "  | _ | | == | | _ |";
      print_endline "  |   | |    | |   |";
      print_endline "  |   | |    | |   |";
      print_endline "  |   | |    | |   |";
      print_endline "  |   | |    | |   |";
      print_endline "  |   | |    | |   |";
      print_endline "  |   | |    | |   |";
      print_endline "  |___|_|____|_|___|"

    method travel_in_time (start : int) (arrival : int) =
      _age <- _age + abs (arrival - start);
      self#_print_tardis ()

    method use_sonic_screwdriver =
      print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"

    method private regenerate () =
      print_endline "Regenerating...";
      _hp <- 100

    method take_damage (amount : int) =
      _hp <- _hp - amount;
      Printf.printf "Doctor %s takes %d damage! HP: %d\n" _name amount _hp;
      if _hp <= 0 then self#regenerate ()

    method is_alive = true
    method get_name = _name

    method attack (target : < take_damage : int -> unit >) =
      print_endline "The Doctor uses the sonic screwdriver!";
      self#use_sonic_screwdriver;
      target#take_damage 25
  end
