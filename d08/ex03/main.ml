let green = "\x1b[32m"
let reset = "\x1b[0m"

let methane = new Alkane.methane
let ethane = new Alkane.ethane
let octane = new Alkane.octane

let molecules = [methane; ethane; octane]

let display_molecule molecule = print_endline molecule#to_string 

let test_alkanes = List.iter display_molecule

let test = test_alkanes molecules

let main () = test

let () = main ()