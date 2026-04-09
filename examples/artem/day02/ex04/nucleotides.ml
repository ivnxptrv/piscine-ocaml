type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | None

type nucleotide = {
  phosphate : phosphate;
  deoxyribose : deoxyribose;
  nucleobase : nucleobase;
}

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

(* Tests *)
let () =
  let print_nucleotide (nucleotide : nucleotide) =
    let nucleobase_name =
      match nucleotide.nucleobase with
      | A -> "adenine"
      | T -> "thymine"
      | C -> "cytosine"
      | G -> "guanine"
      | None -> "unknown"
    in
    Printf.printf "Phosphate: %s\nDeoxyribose: %s\nNucleobase: %s\n"
      nucleotide.phosphate nucleotide.deoxyribose nucleobase_name
  in
  let nucleotide = generate_nucleotide 'A' in
  print_nucleotide nucleotide
