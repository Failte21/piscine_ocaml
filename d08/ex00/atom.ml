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

(* hydrogene h 1
oxygene o 8
carbon c 6
titanium ti 22
barium ba 56
potassium k 19 *)

class hydrogene =
object
  inherit atom
  method name = "hydrogene"
  method symbol = "h"
  method atomic_number = 1
end

class oxygen =
object
  inherit atom
  method name = "oxygen"
  method symbol = "o"
  method atomic_number = 8
end

class carbon =
object
  inherit atom
  method name = "carbon"
  method symbol = "c"
  method atomic_number = 6
end

class titanium =
object
  inherit atom
  method name = "titanium"
  method symbol = "ti"
  method atomic_number = 22
end

class barium =
object
  inherit atom
  method name = "barium"
  method symbol = "ba"
  method atomic_number = 56
end

class potassium =
object
  inherit atom
  method name = "potassium"
  method symbol = "k"
  method atomic_number = 19
end