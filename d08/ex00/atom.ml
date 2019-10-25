(* Forbidden functions : None *)

class virtual atom name symbol atomic_number =
object (self)
  method name = name
  method symbol = symbol
  method atomic_number = atomic_number
  method to_string =
    "Atom " ^ self#name ^ " (symbol: " ^ self#symbol ^ ", " ^ string_of_int self#atomic_number ^ ")"
  method equals (atom: atom) =
    self#atomic_number = atom#atomic_number
end

class hydrogene =
object
  inherit atom "Hydrogene" "H" 1
end

class oxygen =
object
  inherit atom "Oxygen" "O" 8
end

class carbon =
object
  inherit atom "Carbon" "C" 6
end

class titanium =
object
  inherit atom "Titanium" "Ti" 22
end

class barium =
object
  inherit atom "Barium" "Ba" 56
end

class potassium =
object
  inherit atom "Potassium" "K" 19
end

class nitrogen =
object
  inherit atom "Nitrogen" "N" 7
end

class chlorine =
object
  inherit atom "Chlorine" "Cl" 17
end

class bremine =
object
  inherit atom "Bremine" "Br" 35
end