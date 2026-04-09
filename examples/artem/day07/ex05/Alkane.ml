class virtual alkane name n =
  let atoms =
    List.flatten
      [
        List.init n (fun _ -> new Atom.carbon);
        List.init ((2 * n) + 2) (fun _ -> new Atom.hydrogen);
      ]
  in
  object
    inherit Molecule.molecule name atoms
  end

class methane =
  object
    inherit alkane "Methane" 1
  end

class ethane =
  object
    inherit alkane "Ethane" 2
  end

class propane =
  object
    inherit alkane "Propane" 3
  end

class octane =
  object
    inherit alkane "Octane" 8
  end
