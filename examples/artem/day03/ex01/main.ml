let print_value v =
  match v with
  | Some v ->
      Printf.printf "%d - %s - %s\n" (Value.toInt v) (Value.toString v)
        (Value.toStringVerbose v)
  | None -> print_string "None\n"

let rec iterate_values values =
  match values with
  | [] -> ()
  | v :: vs ->
      let next = try Some (Value.next v) with Invalid_argument _ -> None in
      let previous =
        try Some (Value.previous v) with Invalid_argument _ -> None
      in
      print_value (Some v);
      print_string "next: ";
      print_value next;
      print_string "previous: ";
      print_value previous;
      print_string "\n";
      iterate_values vs

let () = iterate_values Value.all
