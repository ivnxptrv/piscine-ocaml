exception Unbalanced

class virtual reaction (start : (Molecule.molecule * int) list)
  (result : (Molecule.molecule * int) list) =
  let count_atoms (side : (Molecule.molecule * int) list) =
    let tbl = Hashtbl.create 16 in
    List.iter
      (fun (mol, qty) ->
        List.iter
          (fun (a : Atom.atom) ->
            let n = a#atomic_number in
            let count = try Hashtbl.find tbl n with Not_found -> 0 in
            Hashtbl.replace tbl n (count + qty))
          mol#atoms)
      side;
    tbl
  in
  let balanced =
    let start_atoms = count_atoms start in
    let result_atoms = count_atoms result in
    let same = ref true in
    Hashtbl.iter
      (fun k v ->
        let other = try Hashtbl.find result_atoms k with Not_found -> 0 in
        if v <> other then same := false)
      start_atoms;
    Hashtbl.iter
      (fun k _ -> if not (Hashtbl.mem start_atoms k) then same := false)
      result_atoms;
    !same
  in
  object
    method get_start =
      if not balanced then raise Unbalanced;
      start

    method get_result =
      if not balanced then raise Unbalanced;
      result

    method is_balanced = balanced
    method virtual balance : reaction
  end
