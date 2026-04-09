class people name =
  object
    val _name = name
    val mutable _hp = 100
    initializer Printf.printf "Initializing %s with %d HP\n" _name _hp
    method to_string = Printf.sprintf "people(name=%s, hp=%d)" _name _hp
    method talk = Printf.printf "I’m %s! Do you know the Doctor?\n" _name

    method die =
      _hp <- 0;
      print_endline "Aaaarghh!"
  end
