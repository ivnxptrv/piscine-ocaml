(* ocamlopt *.ml && ./a.out *)

type phosphate = string

type deoxyribose = string

type nucleobase = A | T | C | G | None

type nucleotide = phosphate * deoxyribose * nucleobase

let generate_nucleotide (nucleobase : char) : nucleotide =
  let nucleobase =
    match nucleobase with
    | 'A' ->
        A
    | 'T' ->
        T
    | 'C' ->
        C
    | 'G' ->
        G
    | _ ->
        None
  in
  ("phosphate", "deoxyribose", nucleobase)

let () =
  let print nucleotide =
    let phosphate, deoxyribose, nucleobase = nucleotide in
    let nucleobase =
      match nucleobase with
      | A ->
          "adenine"
      | T ->
          "thymine"
      | C ->
          "cytosine"
      | G ->
          "guanine"
      | None ->
          "unknown"
    in
    print_endline phosphate ;
    print_endline deoxyribose ;
    print_endline nucleobase ;
    print_endline ""
  in
  print (generate_nucleotide 'A') ;
  print (generate_nucleotide 'B') ;
  print (generate_nucleotide 'C') ;
  print (generate_nucleotide 'Z')
