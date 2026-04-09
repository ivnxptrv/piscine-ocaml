let my_sleep () = Unix.sleep 1

let sleep seconds =
  let remaining = ref seconds in
  while !remaining > 0 do
    ignore (my_sleep ());
    decr remaining
  done

let parse_int s = try Some (int_of_string s) with Failure _ -> None

let () =
  match Sys.argv with
  | [| _; seconds_str |] -> (
      match parse_int seconds_str with
      | Some seconds -> sleep seconds
      | None -> exit 1)
  | _ -> exit 1
