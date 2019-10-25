let green = "\x1b[32m"
let reset = "\x1b[0m"

let water = new Molecule.water
let carbon_dioxyde = new Molecule.carbon_dioxyde
let trinitrotoluene = new Molecule.trinitrotoluene
let methane = new Molecule.methane
let benzene = new Molecule.benzene
let carbon_tetrachloride = new Molecule.carbon_tetrachloride
let bromoethane_13C2 = new Molecule.bromoethane_13C2

let molecules = [water; carbon_dioxyde; trinitrotoluene; methane; benzene; carbon_tetrachloride; bromoethane_13C2]

let display_molecule molecule =
  print_string molecule#name;
  print_string ": ";
  print_endline molecule#formula

let test_molecules = List.iter display_molecule

let test = test_molecules molecules

let main () = test

let () = main ()