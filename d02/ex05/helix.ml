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

type helix = nucleotide list

let char_to_nucleobase = function
  | 'A' -> A
  | 'T' -> T
  | 'C' -> C
  | 'G' -> G
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
  | _ -> "None"

let nucleobase_to_char = function
  | A -> 'A'
  | T -> 'T'
  | C -> 'C'
  | G -> 'G'
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

(* Tests *)

let display_nucleotide nucleotide =
  Printf.printf
    "phosphate: %s, deoxyribose: %s, nucleobase: %s"
    nucleotide.phosphate
    nucleotide.deoxyribose
    (nucleobase_to_string nucleotide.nucleobase)

let display_helix helix =
  let rec display_helix_aux h i =
    match h with
    | [] -> print_char '\n'
    | h::t ->
      Printf.printf "Helix %d:" i;
      display_nucleotide h;
      print_char '\n';
      display_helix_aux t (i + 1) in
  display_helix_aux helix 1

let display_nucleobases = List.iter print_endline

let test n =
  Printf.printf "Test with [%d]:\n" n;
  let helix = generate_helix n in
  Printf.printf "Generated helix:\n"; display_helix helix;
  Printf.printf "helix nucleobases: %s" (helix_to_string helix);
  Printf.printf "\ncomplementary helix:\n"; display_helix (complementary_helix helix)

let main () =
  test 5;
  test 3;
  test 1;
  test 8
  
let () = main ()