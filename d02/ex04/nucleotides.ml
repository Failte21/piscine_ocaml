(* val generate_nucleotide: char -> nucleotide *)

type phosphate = string

type deoxyribose = string

type nucleobase =
  | A
  | T
  | C
  | G
  | None

type nucleotide = {
  phosphate: phosphate;
  deoxyribose: deoxyribose;
  nucleobase: nucleobase
}

let get_nucleobase = function
  | 'A' -> A
  | 'T' -> T
  | 'C' -> C
  | 'G' -> G
  | _ -> None

let nucleobase_to_string = function
| A -> "A"
| T -> "T"
| C -> "C"
| G -> "G"
| _ -> "None"

let generate_nucleotide nucleobase = {
    phosphate = "phosphate";
    deoxyribose = "deoxyribose";
    nucleobase = get_nucleobase(nucleobase)
  }

let display_nucleotide nucleotide =
  Printf.printf
    "phosphate: %s, deoxyribose: %s, nucleobase: %s"
    nucleotide.phosphate
    nucleotide.deoxyribose
    (nucleobase_to_string nucleotide.nucleobase)

let test nucleobase =
  Printf.printf "Test with [%c]:\n" nucleobase;
  let nucleotide = generate_nucleotide nucleobase in
  display_nucleotide nucleotide;
  print_char '\n'

let main () =
  List.iter test ['A'; 'U'; 'T'; 'v'; 'G']

let () = main ()