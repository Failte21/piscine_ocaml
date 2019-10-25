let (--) i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc) in
  aux j []

let carbon = new Atom.carbon
let hydrogene = new Atom.hydrogene

let atoms_from_n n =
  let n_hydrogenes = 2 + (n * 2) in 
  let n_carbons = n in
  let carbon_atoms = List.map(fun _ -> carbon) (0--(n_carbons - 1)) in
  let hydrogene_atoms = List.map(fun _ -> hydrogene) (0--(n_hydrogenes - 1)) in
  carbon_atoms @ hydrogene_atoms

class virtual alkane n name =
  object (this)
    inherit Molecule.molecule (atoms_from_n n) name
    val atoms_from_n =
      let n_hydrogenes = 2 + (n * 2) in 
      let n_carbons = n in
      let carbon_atoms = List.map(fun _ -> carbon) (0--(n_carbons - 1)) in
      let hydrogene_atoms = List.map(fun _ -> hydrogene) (0--(n_hydrogenes - 1)) in
      carbon_atoms @ hydrogene_atoms
  end

class methane =
object
  inherit alkane 1 "Methane" 
end

class ethane =
object
  inherit alkane 2 "Ethane"
end

class octane =
object
  inherit alkane 8 "Octane"
end