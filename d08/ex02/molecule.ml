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
  if (List.length l1 = 1) && (List.nth l1 0) = 'C' then -1
  else if (List.length l2 = 1) && (List.nth l2 0) = 'C' then 1
  else
    if (List.length l1 = 1) && (List.nth l1 0) = 'H' then -1
    else if (List.length l2 = 1) && (List.nth l2 0) = 'H' then 1
    else
      if (List.nth l1 0) = (List.nth l2 0) then
        if ((List.length l1 > 1) && ((List.length l1) = (List.length l2))) then
          (int_of_char (List.nth l1 1)) - (int_of_char (List.nth l2 1))
        else (List.length l1) - (List.length l2)
      else
        (int_of_char (List.nth l1 0)) - (int_of_char (List.nth l2 0))

class virtual molecule (atoms: Atom.atom list) (name: string) =
object (self)
  method atoms = atoms
  method name = name;
  method equals (other_molecule: molecule) = self#formula = other_molecule#formula
  method to_string = "Molecule " ^  self#name ^ ": (formula: " ^ self#formula ^ ")."
  method private sort_symbols = List.sort cmp_s
  method private formula_from_atoms atoms =
    let atom_symbols = List.map (fun atom -> atom#symbol) atoms in
    let sorted_symbols = self#sort_symbols atom_symbols in
    encode sorted_symbols
  method formula = self#formula_from_atoms self#atoms
end

let hydrogene = new Atom.hydrogene
let oxygen = new Atom.oxygen
let carbon = new Atom.carbon
let titanium = new Atom.titanium
let barium = new Atom.barium
let potassium = new Atom.potassium
let nitrogen = new Atom.nitrogen
let chlorine = new Atom.chlorine
let bremine = new Atom.bremine

class water =
object
  inherit molecule [hydrogene; hydrogene; oxygen] "Water"
end

class carbon_dioxyde =
object
  inherit molecule [carbon; oxygen; oxygen] "Carbon dioxyde"
end

class trinitrotoluene =
object
  inherit molecule
  [
    nitrogen; nitrogen; nitrogen;
    hydrogene; hydrogene; hydrogene; hydrogene; hydrogene;
    oxygen; oxygen; oxygen; oxygen; oxygen; oxygen;
    carbon; carbon; carbon; carbon; carbon; carbon; carbon
  ]
  "Trinitrotoluene"
end

class methane =
object
  inherit molecule
  [
    carbon;
    hydrogene; hydrogene; hydrogene; hydrogene
  ]
  "Methane"
end

class benzene =
object
  inherit molecule
  [
    hydrogene; hydrogene; hydrogene; hydrogene; hydrogene; hydrogene;
    carbon; carbon; carbon; carbon; carbon; carbon
  ]
  "Benzene"
end

class carbon_tetrachloride =
object
  inherit molecule [chlorine; carbon; chlorine; chlorine; chlorine] "Carbon tetrachloride"
end

(* C2H5Br *)
(* Bromoethane-13C2 *)

class bromoethane_13C2 =
object
  inherit molecule
  [
    bremine;
    hydrogene; hydrogene; hydrogene; hydrogene; hydrogene;
    carbon; carbon
  ] "Bromoethane-13C2"
end