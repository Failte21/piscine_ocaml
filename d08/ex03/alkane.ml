let (--) i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc) in
  aux j []

let carbon = new Atom.carbon
let hydrogene = new Atom.hydrogene

class virtual alkane =
  object (self)
    inherit Molecule.molecule
    method equals (other_alkane: alkane) = self#formula = other_alkane#formula
    method virtual n: int
    method to_string = "Alkane " ^  self#name ^ ": (formula: " ^ self#formula ^ ")."
    method private atoms =
      let n_hydrogenes = 2 + (self#n * 2) in 
      let n_carbons = self#n in
      let carbon_atoms = List.map(fun _ -> carbon) (0--(n_carbons - 1)) in
      let hydrogene_atoms = List.map(fun _ -> hydrogene) (0--(n_hydrogenes - 1)) in
      carbon_atoms @ hydrogene_atoms
  end

class methane =
object
  inherit alkane
  method name = "Methane"
  method n = 1
end

class ethane =
object
  inherit alkane
  method name = "Ethane"
  method n = 2
end

class octane =
object
  inherit alkane
  method name = "Octane"
  method n = 8
end