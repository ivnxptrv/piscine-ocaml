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

let load_lines filename =
  match open_file filename with
  | Error msg ->
      print_endline msg ; []
  | Ok in_channel -> (
    match read_lines in_channel with
    | Error msg ->
        print_endline msg ; []
    | Ok lines ->
        !lines )

let parse_line line =
  let parts = String.split_on_char ',' line in
  let vector = Array.make (List.length parts - 1) 0.0 in
  let rec parse_line' parts i =
    match parts with
    | cls :: [] ->
        (vector, cls)
    | head :: tail ->
        Array.set vector i (float_of_string head) ;
        parse_line' tail (i + 1)
    | [] ->
        failwith "invalid line format"
  in
  parse_line' parts 0

let rec parse_lines = function
  | [] ->
      []
  | head :: tail ->
      parse_line head :: parse_lines tail

(* each line in a file has array of floats + string all separated by comma *)
(* 1.0,0.5,0.3,g will be converted to ([|1.0; 0.5 ;0.3 |], "g") *)
let examples_of_file (filename : string) : (float array * string) list =
  match load_lines filename with
  | [] ->
      []
  | lines -> (
    try List.rev (parse_lines lines)
    with Failure msg ->
      Printf.printf "Error parsing file: %s\n" msg ;
      [] )

let print_example num (stats, cls) =
  num := !num + 1 ;
  Printf.printf "example %d:\n " !num ;
  Array.iter (fun coordinate -> Printf.printf "%f " coordinate) stats ;
  Printf.printf "-> %s" cls ;
  print_newline ()

let print_examples examples =
  let num = ref 0 in
  List.iter (fun example -> print_example num example) examples

let get_filename =
  match Sys.argv with [|_; filename|] -> Some filename | _ -> None

let () =
  match get_filename with
  | Some filename ->
      print_examples (examples_of_file filename)
  | None ->
      print_string "usage: ./a.out filename"

(* let () = *)
(* let examples = [([|1.1; 2.2|], "g"); ([|1.1; 2.2|], "g")] in *)
(* print_examples examples *)
