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

let k_nn ref_set k radar =
  let radars_with_dist =
    List.map
      (fun radar' ->
        (radar', eu_dist (Radar.vector radar) (Radar.vector radar')))
      ref_set
  in
  let sorted_by_dist =
    List.sort
      (fun (_, dist1) (_, dist2) -> compare dist1 dist2)
      radars_with_dist
  in
  let k_nearest =
    List.take (min k (List.length sorted_by_dist)) sorted_by_dist
  in
  let counts =
    List.fold_left
      (fun acc (r, _) ->
        let cls = Radar.cls r in
        match List.assoc_opt cls acc with
        | Some count -> (cls, count + 1) :: List.remove_assoc cls acc
        | None -> (cls, 1) :: acc)
      [] k_nearest
  in
  let max_count = List.fold_left (fun acc (_, c) -> max acc c) 0 counts in
  let top_classes = List.filter (fun (_, c) -> c = max_count) counts in
  match top_classes with
  | [ (cls, _) ] -> cls
  | _ ->
      let rec find_first_match lst =
        match lst with
        | (r, _) :: tl ->
            let cls = Radar.cls r in
            if List.exists (fun (c, _) -> c = cls) top_classes then cls
            else find_first_match tl
        | [] -> fst (List.hd top_classes)
      in
      find_first_match k_nearest

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
    (fun k ->
      let correct, total =
        List.fold_left
          (fun (correct, total) radar ->
            let predicted_cls = k_nn ref_set k radar in
            let actual_cls = Radar.cls radar in
            if predicted_cls = actual_cls then (correct + 1, total + 1)
            else (correct, total + 1))
          (0, 0) test_set
      in
      let accuracy = float_of_int correct /. float_of_int total *. 100.0 in
      Printf.printf "K-NN (k=%d) Accuracy: %d/%d (%.2f%%)\n" k correct total
        accuracy)
    [ 1; 2; 3; 5; 7; 9; 11 ]
