let examples_of_file filename =
  let parse_line line =
    let parts = String.split_on_char ',' line in
    let vector = Array.make (List.length parts - 1) 0.0 in
    let rec parse_line' parts i =
      match parts with
      | cls :: [] -> (vector, cls)
      | hd :: tl ->
          Array.set vector i (float_of_string hd);
          parse_line' tl (i + 1)
      | [] -> failwith "Invalid line format"
    in
    parse_line' parts 0
  in
  let rec read_lines ic acc =
    try
      let line = input_line ic in
      read_lines ic (parse_line line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  let ic = open_in filename in
  read_lines ic []

let () =
  let examples = examples_of_file "ionosphere.train.csv" in
  List.iter
    (fun (vector, cls) ->
      Printf.printf "%s — %s\n"
        (String.concat "," (List.map string_of_float (Array.to_list vector)))
        cls)
    examples
