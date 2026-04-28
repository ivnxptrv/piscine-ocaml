class dalek =
  (* creates high entropy randomness engine *)
  let seed = Random.State.make_self_init () in
  let generate_postfix len =
    String.capitalize_ascii
      (String.init len (fun _ ->
           char_of_int (Random.State.int_in_range seed ~min:97 ~max:122) ) )
  in
  let messages =
    [| "Explain! Explain!"
     ; "Exterminate! Exterminate!"
     ; "I obey!"
     ; "You are the Doctor! You are the enemy of the Daleks!" |]
  in
  object (self)
    val _name = "Dalek" ^ generate_postfix 3

    val mutable _hp = 100

    (* changes value each time exterminate method is used *)
    val mutable _shield = true

    method to_string =
      "dalek(name= " ^ _name ^ ", hp= " ^ string_of_int _hp ^ ", shield= "
      ^ string_of_bool _shield ^ ")"

    method talk =
      let index = Random.State.int_in_range seed ~min:0 ~max:3 in
      let msg = messages.(index) in
      print_endline msg

    method exterminate (people : People.people) =
      _shield <- false ;
      people#die

    method die : unit =
      _hp <- 0 ;
      print_endline "Emergency Temporal Shift!"

    method repair =
      _hp <- 100 ;
      _shield <- true ;
      print_endline "Dalek is repaired!"

    method take_damage (amount : int) =
      if _shield then (
        print_endline "Shield absorbed the attack!" ;
        _shield <- false )
      else (
        _hp <- _hp - amount ;
        Printf.printf "%s takes %d damage! HP: %d\n" _name amount _hp ;
        if _hp <= 0 then self#die )
  end
