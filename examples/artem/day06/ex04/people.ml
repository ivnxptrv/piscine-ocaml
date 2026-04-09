class people name =
  object (self)
    val _name = name
    val mutable _hp = 100
    initializer Printf.printf "Initializing %s with %d HP\n" _name _hp
    method to_string = Printf.sprintf "people(name=%s, hp=%d)" _name _hp
    method talk = Printf.printf "I’m %s! Do you know the Doctor?\n" _name
    method private die = print_endline "Aaaarghh!"

    method take_damage (amount : int) =
      _hp <- _hp - amount;
      Printf.printf "%s takes %d damage! HP: %d\n" _name amount _hp;
      if _hp <= 0 then self#die

    method is_alive = _hp > 0
    method get_name = _name
  end
