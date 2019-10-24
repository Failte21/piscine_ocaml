let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let encode l =
  let rec encode_aux list element n acc =
    match list with
    | [] ->
      if n > 1 then acc ^ element ^ (string_of_int n) else acc ^ element
    | h::t when h = element -> encode_aux t element (n + 1) acc
    | h::t ->
      if n > 1 then encode_aux t h 1 (acc ^ element ^ (string_of_int n))
      else encode_aux t h 1 (acc ^ element)
  in
  match l with
    | [] -> ""
    | h::t -> encode_aux (h::t) h 0 ""

let cmp_s s1 s2 =
  let l1 = explode s1 in
  let l2 = explode s2 in
  if (List.nth l1 0) = (List.nth l2 0) then
    if ((List.length l1 > 1) && ((List.length l1) = (List.length l2))) then
      (int_of_char (List.nth l1 1)) - (int_of_char (List.nth l2 1))
    else (List.length l1) - (List.length l2)
  else
    (int_of_char (List.nth l1 0)) - (int_of_char (List.nth l2 0))

class virtual molecule =
object (self)
  val virtual atoms: Atom.atom list
  method atoms: Atom.atom list = atoms
  method private sort_symbols = List.sort cmp_s
  method virtual name: string
  method formula =
    let atom_symbols = List.map (fun atom -> atom#symbol) self#atoms in
    let sorted_symbols = self#sort_symbols atom_symbols in
    encode sorted_symbols


end

let hydrogene = new Atom.hydrogene
let oxygen = new Atom.oxygen
let carbon = new Atom.carbon
let titanium = new Atom.titanium
let barium = new Atom.barium
let potassium = new Atom.potassium
let nitrogen = new Atom.nitrogen

(* • Water (H2O))
• Carbon dioxyde (CO2) *)

(* Trinitrotoluene *)
(* 3 atoms of Nitrogen
• 5 atoms of Hydrogen
• 6 atoms of Oxygen
• 7 atoms of Carbon *)

class water =
object
  inherit molecule
  val atoms = [hydrogene; hydrogene; oxygen]
  method name = "Water"
end

class carbon_dioxyde =
object
  inherit molecule
  val atoms = [carbon; oxygen; oxygen]
  method name = "Carbon dioxyde"
end

class trinitrotoluene =
object
  inherit molecule
  val atoms = [
    nitrogen; nitrogen; nitrogen;
    hydrogene; hydrogene; hydrogene; hydrogene; hydrogene;
    oxygen; oxygen; oxygen; oxygen; oxygen; oxygen;
    carbon; carbon; carbon; carbon; carbon; carbon; carbon
  ]
  method name = "Trinitrotoluene"
end

class methane =
object
  inherit molecule
  val atoms = [
    carbon;
    hydrogene; hydrogene; hydrogene; hydrogene
  ]
  method name = "Methane"
end

class benzene =
object
  inherit molecule
  val atoms = [
    hydrogene; hydrogene; hydrogene; hydrogene; hydrogene; hydrogene;
    carbon; carbon; carbon; carbon; carbon; carbon
  ]
  method name = "Benzene"
end