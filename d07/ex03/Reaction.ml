class virtual reaction (start : (Molecule.molecule * int) list)
  (result : (Molecule.molecule * int) list) =
  object (self)
    method virtual balance : reaction

    (* must be FUNCTIONAL *)
    (* (\* Helper to count all atoms in a side *\) *)
    (* method private count_atoms (side : (Molecule.molecule * int) list) = *)
    (*   let counts = Hashtbl.create 16 in *)
    (*   List.iter *)
    (*     (fun (mol, coeff) -> *)
    (*       (\* You need a way to get the atoms from the molecule *\) *)
    (*       List.iter *)
    (*         (fun atom -> *)
    (*           let sym = atom#symbol in *)
    (*           let old_count = *)
    (*             try Hashtbl.find counts sym with Not_found -> 0 *)
    (*           in *)
    (*           Hashtbl.replace counts sym (old_count + coeff) ) *)
    (*         mol#atoms (\* Assumes molecule has a method 'atoms' *\) ) *)
    (*     side ; *)
    (*   counts *)
    (* method private compare_counts h1 h2 = *)
    (* if Hashtbl.length h1 <> Hashtbl.length h2 then false *)
    (* else *)
    (*     Hashtbl.fold (fun sym count acc -> *)
    (*     acc && (try Hashtbl.find h2 sym = count with Not_found -> false) *)
    (*     ) h1 true *)

    method is_balanced =
      let start_counts = self#count_atoms start in
      let result_counts = self#count_atoms result in
      (* Logic to compare two hash tables *)
      self#compare_counts start_counts result_counts
  end
(* balanced if num and type of atoms same at start and at the end *)
(* TODO: we dont need rais exception in get_start as it could be unbalanced*)
(* let result = (my_reaction#balance)#is_balanced *)
