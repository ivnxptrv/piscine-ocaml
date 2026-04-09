class dalek =
  let rng = Random.State.make_self_init () in
  let generate_name len =
    String.capitalize_ascii
      (String.init len (fun _ ->
           char_of_int (Random.State.int_in_range rng ~min:97 ~max:122)))
  in
  let messages =
    [|
      "Explain! Explain!";
      "Exterminate! Exterminate!";
      "I obey!";
      "You are the Doctor! You are the enemy of the Daleks!";
    |]
  in
  object (self)
    val _name = "Dalek" ^ generate_name 3
    val mutable _hp = 100
    val mutable _shield = true

    method to_string =
      "dalek(name=" ^ _name ^ ", hp=" ^ string_of_int _hp ^ ", shield="
      ^ string_of_bool _shield ^ ")"

    method talk =
      let i =
        Random.State.int_in_range rng ~min:0 ~max:(Array.length messages - 1)
      in
      print_endline messages.(i)

    method exterminate (people : People.people) =
      people#take_damage 100;
      _shield <- not _shield

    method private die = print_endline "Emergency Temporal Shift!"

    method repair =
      _hp <- 100;
      _shield <- true;
      print_endline "Dalek is repaired!"

    method take_damage (amount : int) =
      if _shield then (
        print_endline "Shield absorbed the attack!";
        _shield <- false)
      else (
        _hp <- _hp - amount;
        Printf.printf "%s takes %d damage! HP: %d\n" _name amount _hp;
        if _hp <= 0 then self#die)
  end
