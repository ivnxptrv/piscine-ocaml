let my_sleep () = Unix.sleep 1

let sleep seconds =
  for i = 1 to seconds do
    ignore (my_sleep ())
  done

let parse_int s = try Ok (int_of_string s) with Failure str -> Error str

let () =
  match Sys.argv with
  | [|_; seconds|] -> (
    match parse_int seconds with
    | Ok seconds ->
        sleep seconds
    | Error str ->
        exit 1 )
  | _ ->
      exit 1
