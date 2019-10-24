(* Forbidden functions : None *)

class virtual atom =
object (self)
  method virtual name: string
  method virtual symbol: string
  method virtual atomic_number: int
  method to_string =
    Printf.printf "Atom %s (symbol: %s, atomic number %d)\n" self#name self#symbol self#atomic_number
  method equals (atom: atom) =
    self#atomic_number = atom#atomic_number
end

class hydrogene =
object
  inherit atom
  method name = "Hydrogene"
  method symbol = "H"
  method atomic_number = 1
end

class oxygen =
object
  inherit atom
  method name = "Oxygen"
  method symbol = "O"
  method atomic_number = 8
end

class carbon =
object
  inherit atom
  method name = "Carbon"
  method symbol = "C"
  method atomic_number = 6
end

class titanium =
object
  inherit atom
  method name = "Titanium"
  method symbol = "Ti"
  method atomic_number = 22
end

class barium =
object
  inherit atom
  method name = "Barium"
  method symbol = "Ba"
  method atomic_number = 56
end

class potassium =
object
  inherit atom
  method name = "Potassium"
  method symbol = "K"
  method atomic_number = 19
end

class nitrogen =
object
  inherit atom
  method name = "Nitrogen"
  method symbol = "N"
  method atomic_number = 7
end