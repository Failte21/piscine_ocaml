type phosphate = string

type deoxyribose = string

type nucleobase =
  | A
  | T
  | C
  | G
  | U
  | None

type nucleotide = {
  phosphate: phosphate;
  deoxyribose: deoxyribose;
  nucleobase: nucleobase
}

type helix = nucleotide list

type rna = nucleobase list

let char_to_nucleobase = function
| 'A' -> A
  | 'T' -> T
  | 'C' -> C
  | 'G' -> G
  | 'U' -> U
  | _ -> None

let get_nucleobase_char = function
  | 0 -> 'A'
  | 1 -> 'T'
  | 2 -> 'C'
  | 3 -> 'G'
  | _ -> '_'

let generate_nucleotide nucleobase = {
  phosphate = "phosphate";
  deoxyribose = "deoxyribose";
  nucleobase = char_to_nucleobase nucleobase
}

let nucleobase_to_string = function
  | A -> "A"
  | T -> "T"
  | C -> "C"
  | G -> "G"
  | U -> "U"
  | _ -> "None"

let nucleobase_to_char = function
  | A -> 'A'
  | T -> 'T'
  | C -> 'C'
  | G -> 'G'
  | U -> 'U'
  | _ -> '_'

let nucleobase_to_complementary = function
  | A -> T
  | T -> A
  | C -> G
  | G -> C
  | _ -> None

let nucleobase_to_complementary_char nucleobase =
  nucleobase_to_char (nucleobase_to_complementary nucleobase)

let nucleotide_from_complementary nucleobase =
  generate_nucleotide (nucleobase_to_complementary_char nucleobase)

(* val generate_helix : int -> helix. *)
let rec generate_helix n: helix =
  if n <= 0 then []
  else (generate_nucleotide (get_nucleobase_char (Random.int 4))) :: (generate_helix (n - 1))
  
(* val helix_to_string : helix -> string. *)
let rec helix_to_string (l: helix) =
  match l with
  | h::t -> (nucleobase_to_string h.nucleobase) ^ (helix_to_string t)
  | [] -> ""

(* val complementary_helix : helix -> helix *)
let rec complementary_helix (l: helix): helix =
  match l with
  | h::t -> (nucleotide_from_complementary h.nucleobase) :: (complementary_helix t)
  | [] -> []

let rec helix_to_nucleobases = function
  | h::t -> h.nucleobase :: helix_to_nucleobases t
  | [] -> []

let rec nucleobases_h_to_rna = function
  | h::t -> (if h = T then U else h) :: nucleobases_h_to_rna t
  | [] -> []

(* val generate_rna: helix -> rna *)
let generate_rna (helix: helix): rna =
  let c_helix = complementary_helix helix in
  let nucleobases = helix_to_nucleobases c_helix in
  nucleobases_h_to_rna nucleobases

(* Tests *)

let display_nucleotide nucleotide =
  print_string "phosphate: ";
  print_endline nucleotide.phosphate;
  print_string "deoxyribose: ";
  print_endline nucleotide.deoxyribose;
  print_string "nucleobase: ";
  print_endline (nucleobase_to_string nucleotide.nucleobase)

let display_helix helix =
  let rec display_helix_aux h i =
    match h with
    | [] -> print_char '\n'
    | h::t ->
      print_string "Helix ";
      print_int i;
      print_char '\n';
      display_nucleotide h;
      display_helix_aux t (i + 1) in
  display_helix_aux helix 1

let display_nucleobases = List.iter print_endline

let test n =
  print_string "Test with ";
  print_string "[";
  print_int n;
  print_endline "]: ";
  let helix = generate_helix n in
  let c_helix = complementary_helix helix in
  print_string "helix nucleobases: ";
  List.iter (fun e -> print_char (nucleobase_to_char e)) (helix_to_nucleobases helix);
  print_string "\ncomplementary helix nucleobases: ";
  List.iter (fun e -> print_char (nucleobase_to_char e)) (helix_to_nucleobases c_helix);
  print_string "\nrna: ";
  List.iter (fun e -> print_char (nucleobase_to_char e)) (generate_rna helix);
  print_char '\n'

let main () =
  test 5;
  test 3;
  test 1;
  test (-1)
  
let () = main ()