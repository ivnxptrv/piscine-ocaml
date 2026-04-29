(* your constructor will only accept a name and a list of atoms  *)
class virtual molecule name (atoms : Atom.atom list) =
  object (self)
    method atoms = atoms

    method name = name

    method to_string = Printf.sprintf "molecule %s %s" name self#formula

    method equals (other : molecule) = self#formula = other#formula

    (* C7H5N3O6 *)
    method formula =
      (* create container  *)
      let tbl = Hashtbl.create (List.length atoms) in
      (* init container with given list items counting how many times it appeared *)
      List.iter
        (fun a ->
          let sym = a#symbol in
          let count = try Hashtbl.find tbl sym with Not_found -> 0 in
          Hashtbl.replace tbl sym (count + 1) )
        atoms ;
      (* making list of tuples *)
      let pairs = Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl [] in
      (* compare func for sorting *)
      let hill_order (s1, _) (s2, _) =
        match (s1, s2) with
        | "C", _ ->
            -1
        | _, "C" ->
            1
        | "H", _ ->
            -1
        | _, "H" ->
            1
        | _ ->
            String.compare s1 s2
      in
      (* sort by Merge Sort *)
      let sorted = List.sort hill_order pairs in
      (* cretae final string in hill order *)
      String.concat ""
        (List.map
           (fun (sym, n) -> if n = 1 then sym else sym ^ string_of_int n)
           sorted )
  end

class water =
  object
    inherit
      molecule
        "water"
        (new Atom.oxygen :: List.init 2 (fun _ -> new Atom.hydrogen))
  end

class carbon_dioxide =
  object
    inherit
      molecule
        "carbon dioxide"
        (List.flatten
           [ List.init 1 (fun _ -> new Atom.carbon)
           ; List.init 2 (fun _ -> new Atom.oxygen) ] )
  end

class methane =
  object
    inherit
      molecule
        "methane"
        (List.flatten
           [ List.init 1 (fun _ -> new Atom.carbon)
           ; List.init 4 (fun _ -> new Atom.hydrogen) ] )
  end

class ammonia =
  object
    inherit
      molecule
        "ammonia"
        (List.flatten
           [ List.init 1 (fun _ -> new Atom.nitrogen)
           ; List.init 3 (fun _ -> new Atom.hydrogen) ] )
  end

class ethanol =
  object
    inherit
      molecule
        "ethanol"
        (List.flatten
           [ List.init 2 (fun _ -> new Atom.carbon)
           ; List.init 6 (fun _ -> new Atom.hydrogen)
           ; List.init 1 (fun _ -> new Atom.oxygen) ] )
  end
