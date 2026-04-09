type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | None

type nucleotide = {
  phosphate : phosphate;
  deoxyribose : deoxyribose;
  nucleobase : nucleobase;
}

type helix = nucleotide list

let generate_nucleotide (nucleobase_char : char) : nucleotide =
  let nucleobase =
    match nucleobase_char with
    | 'A' -> A
    | 'T' -> T
    | 'C' -> C
    | 'G' -> G
    | _ -> None
  in
  { phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase }

let generate_helix (n : int) : helix =
  let random_state = Random.State.make_self_init () in
  let rec generate_nucleotide_list n =
    if n <= 0 then []
    else
      let nucleobase =
        match Random.State.int random_state 4 with
        | 0 -> 'A'
        | 1 -> 'T'
        | 2 -> 'C'
        | _ -> 'G'
      in
      generate_nucleotide nucleobase :: generate_nucleotide_list (n - 1)
  in
  generate_nucleotide_list n

let helix_to_string helix =
  let nucleotide_to_nucleobase_letter nucleotide =
    match nucleotide.nucleobase with
    | A -> "A"
    | T -> "T"
    | C -> "C"
    | G -> "G"
    | _ -> raise (Invalid_argument "Invalid nucleobase")
  in
  let rec string_of_nucleotide_list = function
    | [] -> ""
    | n :: ns ->
        nucleotide_to_nucleobase_letter n ^ string_of_nucleotide_list ns
  in
  string_of_nucleotide_list helix

let complementary_helix (h : helix) : helix =
  let rec complement_nucleotide_list = function
    | [] -> []
    | n :: ns ->
        let complement =
          match n.nucleobase with
          | A -> 'T'
          | T -> 'A'
          | C -> 'G'
          | G -> 'C'
          | None -> raise (Invalid_argument "Invalid nucleobase")
        in
        generate_nucleotide complement :: complement_nucleotide_list ns
  in
  complement_nucleotide_list h

(* Tests *)
let () =
  let helix = generate_helix 10 in
  let complementary_helix = complementary_helix helix in
  print_endline (helix_to_string helix);
  print_endline (helix_to_string complementary_helix)
