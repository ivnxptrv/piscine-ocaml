module Radar = struct
  type t = float array * string

  let vector = fst
  let cls = snd

  let to_string (vector, cls) =
    Printf.sprintf "%s — %s"
      (String.concat "," (List.map string_of_float (Array.to_list vector)))
      cls
end

let eu_dist a b =
  let sum = ref 0.0 in
  Array.iter2 (fun x y -> sum := !sum +. ((x -. y) ** 2.0)) a b;
  sqrt !sum

let one_nn ref_set radar =
  let cmp (n_radar, min_dist) radar' =
    let dist = eu_dist (Radar.vector radar) (Radar.vector radar') in
    if dist < min_dist then (radar', dist) else (n_radar, min_dist)
  in
  let n_radar, _ = List.fold_left cmp (List.hd ref_set, infinity) ref_set in
  Radar.cls n_radar

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
  let ref_set = examples_of_file "ionosphere.train.csv" in
  let test_set = examples_of_file "ionosphere.test.csv" in
  List.iter
    (fun radar ->
      let t = one_nn ref_set radar in
      Printf.printf "Testing radar:\n%s\nGuessed type: %s\n\n"
        (Radar.to_string radar) t)
    test_set
