type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | U | None

type nucleotide = {
  phosphate : phosphate;
  deoxyribose : deoxyribose;
  nucleobase : nucleobase;
}

type helix = nucleotide list
type rna = nucleobase list

type aminoacid =
  | Stop
  | Ala
  | Arg
  | Asn
  | Asp
  | Cys
  | Gln
  | Glu
  | Gly
  | His
  | Ile
  | Leu
  | Lys
  | Met
  | Phe
  | Pro
  | Ser
  | Thr
  | Trp
  | Tyr
  | Val

type protein = aminoacid list

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

let generate_helix ?(seed : string option = None) (n : int) : helix =
  let seed_to_int_array (s : string) : int array =
    let len = String.length s in
    if len = 0 then [| 0 |] else Array.init len (fun i -> Char.code s.[i])
  in
  let random_state =
    match seed with
    | Some s -> Random.State.make (seed_to_int_array s)
    | None -> Random.State.make_self_init ()
  in
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
          | _ -> raise (Invalid_argument "Invalid nucleobase")
        in
        generate_nucleotide complement :: complement_nucleotide_list ns
  in
  complement_nucleotide_list h

let generate_rna (h : helix) : rna =
  let rec generate_rna_list = function
    | [] -> []
    | n :: ns ->
        let complement =
          match n.nucleobase with
          | A -> U
          | T -> A
          | C -> G
          | G -> C
          | _ -> raise (Invalid_argument "Invalid nucleobase")
        in
        complement :: generate_rna_list ns
  in
  generate_rna_list h

let generate_bases_triplets (rna : rna) :
    (nucleobase * nucleobase * nucleobase) list =
  let rec generate_triplets = function
    | n1 :: n2 :: n3 :: ns -> (n1, n2, n3) :: generate_triplets ns
    | _ -> []
  in
  generate_triplets rna

let rec string_of_protein (p : protein) : string =
  let aminoacid_to_string aminoacid =
    match aminoacid with
    | Stop -> "End of translation"
    | Ala -> "Alanine"
    | Arg -> "Arginine"
    | Asn -> "Asparagine"
    | Asp -> "Aspartique"
    | Cys -> "Cysteine"
    | Gln -> "Glutamine"
    | Glu -> "Glutamique"
    | Gly -> "Glycine"
    | His -> "Histidine"
    | Ile -> "Isoleucine"
    | Leu -> "Leucine"
    | Lys -> "Lysine"
    | Met -> "Methionine"
    | Phe -> "Phenylalanine"
    | Pro -> "Proline"
    | Ser -> "Serine"
    | Thr -> "Threonine"
    | Trp -> "Tryptophane"
    | Tyr -> "Tyrosine"
    | Val -> "Valine"
  in
  match p with
  | [] -> ""
  | h :: [] -> aminoacid_to_string h
  | h :: t -> aminoacid_to_string h ^ ", " ^ string_of_protein t

let decode_arn (rna : rna) : protein =
  let triplet_to_aminoacid = function
    | U, A, A | U, A, G | U, G, A -> Stop
    | G, C, A | G, C, C | G, C, G | G, C, U -> Ala
    | A, G, A | A, G, G | C, G, A | C, G, C | C, G, G | C, G, U -> Arg
    | A, A, C | A, A, U -> Asn
    | G, A, C | G, A, U -> Asp
    | U, G, C | U, G, U -> Cys
    | C, A, A | C, A, G -> Gln
    | G, A, A | G, A, G -> Glu
    | G, G, A | G, G, C | G, G, G | G, G, U -> Gly
    | C, A, C | C, A, U -> His
    | A, U, A | A, U, C | A, U, U -> Ile
    | C, U, A | C, U, C | C, U, G | C, U, U | U, U, A | U, U, G -> Leu
    | A, A, A | A, A, G -> Lys
    | A, U, G -> Met
    | U, U, C | U, U, U -> Phe
    | C, C, C | C, C, A | C, C, G | C, C, U -> Pro
    | U, C, A | U, C, C | U, C, G | U, C, U | A, G, U | A, G, C -> Ser
    | A, C, A | A, C, C | A, C, G | A, C, U -> Thr
    | U, G, G -> Trp
    | U, A, C | U, A, U -> Tyr
    | G, U, A | G, U, C | G, U, G | G, U, U -> Val
    | _ -> raise (Invalid_argument "Invalid triplet")
  in
  let rec decode_triplets triplets =
    match triplets with
    | [] -> []
    | t :: ns -> (
        let aminoacid = triplet_to_aminoacid t in
        match aminoacid with
        | Stop -> [ Stop ]
        | _ -> aminoacid :: decode_triplets ns)
  in
  decode_triplets (generate_bases_triplets rna)

let make_life (seed : string) =
  let rec print_rna (rna : rna) =
    let nucleotide_to_nucleobase_letter nucleobase =
      match nucleobase with
      | A -> 'A'
      | U -> 'U'
      | C -> 'C'
      | G -> 'G'
      | _ -> raise (Invalid_argument "Invalid nucleobase")
    in
    match rna with
    | [] -> print_newline ()
    | n :: ns ->
        print_char (nucleotide_to_nucleobase_letter n);
        print_rna ns
  in
  let min_len = 10 in
  let helix_len =
    let len = String.length seed in
    if len < min_len then min_len else len
  in
  let helix = generate_helix helix_len ~seed:(Some seed) in
  print_endline ("Helix: " ^ helix_to_string helix);
  let complement = complementary_helix helix in
  print_endline ("Complementary helix: " ^ helix_to_string complement);
  let rna = generate_rna helix in
  print_string "RNA: ";
  print_rna rna;
  let protein = decode_arn rna in
  print_endline ("Protein: " ^ string_of_protein protein)

(* Tests *)
let () = make_life "hello, world! rise and shine"
