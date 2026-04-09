class virtual atom name symbol atomic_number =
  object
    method name = name
    method symbol = symbol
    method atomic_number = atomic_number

    method to_string =
      Printf.sprintf "atom %s (%s) %d" name symbol atomic_number

    method equals (other : atom) = atomic_number = other#atomic_number
  end

class hydrogen =
  object
    inherit atom "Hydrogen" "H" 1
  end

class carbon =
  object
    inherit atom "Carbon" "C" 6
  end

class oxygen =
  object
    inherit atom "Oxygen" "O" 8
  end

class nitrogen =
  object
    inherit atom "Nitrogen" "N" 7
  end

class helium =
  object
    inherit atom "Helium" "He" 2
  end

class iron =
  object
    inherit atom "Iron" "Fe" 26
  end
