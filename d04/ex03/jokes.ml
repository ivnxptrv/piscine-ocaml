let open_file filename =
  try
    let in_channel = open_in filename in
    Ok in_channel
  with Sys_error msg -> Error (Printf.sprintf "Error opening file: %s\n" msg)

let read_lines in_channel =
  let lines = ref [] in
  try
    while true do
      lines := input_line in_channel :: !lines
    done
  with
  | Sys_error msg ->
      close_in in_channel ;
      Error (Printf.sprintf "Error reading file: %s\n" msg)
  | End_of_file ->
      close_in in_channel ; Ok lines

let load_jokes filename =
  match open_file filename with
  | Error msg ->
      print_endline msg ; [||]
  | Ok in_channel -> (
    match read_lines in_channel with
    | Error msg ->
        print_endline msg ; [||]
    | Ok lines ->
        Array.of_list !lines )

let get_filename =
  match Sys.argv with [|_; filename|] -> Some filename | _ -> None

let print_random_joke jokes =
  if Array.length jokes = 0 then ()
  else begin
    Random.self_init () ;
    let index = Random.int (Array.length jokes) in
    print_endline jokes.(index)
  end

let () =
  match get_filename with
  | Some filename ->
      print_random_joke (load_jokes filename)
  | None ->
      print_string "usage: ./a.out filename"
