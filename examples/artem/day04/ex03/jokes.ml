let load_jokes filename =
  let ic = open_in filename in
  let jokes = ref [] in
  try
    while true do
      jokes := input_line ic :: !jokes
    done
  with End_of_file ->
    close_in ic;
    Array.of_list !jokes

let get_random_joke jokes =
  let rnd = Random.State.make_self_init () in
  jokes.(Random.State.int rnd (Array.length jokes))

let () =
  match Sys.argv with
  | [| _; filename |] -> (
      try
        begin
          let jokes = load_jokes filename in
          let joke = get_random_joke jokes in
          print_endline joke
        end
      with Sys_error msg -> print_endline ("Error: " ^ msg))
  | _ -> print_endline "Usage: ./ex03 filename"
