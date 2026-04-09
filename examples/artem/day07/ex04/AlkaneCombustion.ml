let rec gcd a b = if b = 0 then a else gcd b (a mod b)

let gcd_list = function
  | [] -> 1
  | x :: xs -> List.fold_left (fun acc v -> gcd acc v) x xs

(* Deduplicate alkanes by formula, summing their counts *)
let dedup_alkanes (alkanes : Alkane.alkane list) =
  let tbl = Hashtbl.create 16 in
  let order = ref [] in
  List.iter
    (fun (a : Alkane.alkane) ->
      let f = a#formula in
      let count = try Hashtbl.find tbl f with Not_found -> 0 in
      if count = 0 then order := a :: !order;
      Hashtbl.replace tbl f (count + 1))
    alkanes;
  List.rev_map
    (fun (a : Alkane.alkane) -> (a, Hashtbl.find tbl a#formula))
    !order

(* Count carbons in an alkane *)
let count_c (a : Alkane.alkane) =
  List.length (List.filter (fun (at : Atom.atom) -> at#symbol = "C") a#atoms)

(*
   Alkane combustion: CnH(2n+2) + O2 -> CO2 + H2O

   Unbalanced per unit:
     CnH(2n+2) + ((3n+1)/2) O2 -> n CO2 + (n+1) H2O

   To keep integers, work with doubled coefficients:
     2 CnH(2n+2) + (3n+1) O2 -> 2n CO2 + 2(n+1) H2O

   With quantity q:
     2q CnH(2n+2) + q(3n+1) O2 -> 2qn CO2 + 2q(n+1) H2O

   Then sum across all alkanes and divide by GCD.
*)
let compute_balanced (alkanes : (Alkane.alkane * int) list) =
  let total_o2 = ref 0 in
  let total_co2 = ref 0 in
  let total_h2o = ref 0 in
  let alkane_coeffs =
    List.map
      (fun (a, q) ->
        let n = count_c a in
        let coeff = 2 * q in
        total_o2 := !total_o2 + (q * ((3 * n) + 1));
        total_co2 := !total_co2 + (2 * q * n);
        total_h2o := !total_h2o + (2 * q * (n + 1));
        (a, coeff))
      alkanes
  in
  let all_coeffs =
    List.map snd alkane_coeffs @ [ !total_o2; !total_co2; !total_h2o ]
  in
  let g = gcd_list all_coeffs in
  let start_mols : (Molecule.molecule * int) list =
    List.map (fun (a, c) -> ((a :> Molecule.molecule), c / g)) alkane_coeffs
    @ [ ((new Molecule.dioxygen :> Molecule.molecule), !total_o2 / g) ]
  in
  let result_mols : (Molecule.molecule * int) list =
    [
      ((new Molecule.carbon_dioxide :> Molecule.molecule), !total_co2 / g);
      ((new Molecule.water :> Molecule.molecule), !total_h2o / g);
    ]
  in
  (start_mols, result_mols)

class alkane_combustion (alkanes : Alkane.alkane list) =
  let deduped = dedup_alkanes alkanes in
  let start_mols : (Molecule.molecule * int) list =
    List.map (fun (a, q) -> ((a :> Molecule.molecule), q)) deduped
    @ [ ((new Molecule.dioxygen :> Molecule.molecule), 0) ]
  in
  let result_mols : (Molecule.molecule * int) list =
    [
      ((new Molecule.carbon_dioxide :> Molecule.molecule), 0);
      ((new Molecule.water :> Molecule.molecule), 0);
    ]
  in
  let balanced_start, balanced_result = compute_balanced deduped in
  object
    inherit Reaction.reaction start_mols result_mols

    method balance =
      (object (self)
         inherit Reaction.reaction balanced_start balanced_result
         method balance = (self :> Reaction.reaction)
       end
        :> Reaction.reaction)
  end
