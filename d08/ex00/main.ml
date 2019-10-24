let green = "\x1b[32m"
let reset = "\x1b[0m"

let hydrogene = new Atom.hydrogene
let oxygen = new Atom.oxygen
let carbon = new Atom.carbon
let titanium = new Atom.titanium
let barium = new Atom.barium
let potassium = new Atom.potassium

let atoms = [hydrogene; oxygen; carbon; titanium; barium; potassium]

let describe = List.iter (fun atom -> atom#to_string) atoms

let compare a1 a2 =
  Printf.printf "%s = %s: %B\n" a1#name a2#name (a1#equals a2);
  a2

let compare_all =
  print_endline "...";
  List.fold_left compare hydrogene atoms;
  print_endline "..."


let test =
  describe;
  compare_all

let main () = test

let () = main ()