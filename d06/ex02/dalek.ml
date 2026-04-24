class dalek =
  (* creates high entropy randomness engine *)
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
    (* TODO: finish random generation *)
    val _name = "DalekXXX"

    val mutable _hp = 100

    (* changes value each time exterminate method is used *)
    val mutable _shield = true

    method to_string =
      "dalek(name= " ^ _name ^ ", hp= " ^ string_of_int _hp ^ ", shield= "
      ^ string_of_bool _shield ^ ")"

    method talk = 
  end
