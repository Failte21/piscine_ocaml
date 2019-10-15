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

let get_nucleobase nucleobase =
  match nucleobase with
  | 'A' -> A
  | 'T' -> T
  | 'C' -> C
  | 'G' -> G
  | _ -> None

let generate_nucleotide nucleobase = {
    phosphate = "phosphate";
    deoxyribose = "deoxyribose";
    nucleobase = get_nucleobase(nucleobase)
  }

let display_nucleotide nucleotide =
  print_string "phosphate: ";
  print_endline nucleotide.phosphate;
  print_string "deoxyribose: ";
  print_endline nucleotide.deoxyribose

let test nucleobase =
  print_string "Test with ";
  print_string "[";
  print_char nucleobase;
  print_string "]:\n";
  let nucleotide = generate_nucleotide nucleobase in
  display_nucleotide nucleotide

let main () =
  test 'A'

let () = main ()