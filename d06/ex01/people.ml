class people name =
  object
    val _name : string = name

    val mutable _hp : int = 100

    initializer Printf.printf "Initializing %s with %d HP\n" _name _hp

    method to_string : string =
      Printf.sprintf "people(name=%s, hp=%d)" _name _hp

    method talk : unit = Printf.printf "I’m %s! Do you know the Doctor?\n" _name

    method die : unit =
      _hp <- 0 ;
      print_endline "Aaaarghh!"
  end
