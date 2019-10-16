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

type aminoacid =
  | Stop
  | Ala
  | Arg
  | Asn
  | Asp
  | Cys
  | Gln
  | Glu
  | Gly
  | His
  | Ile
  | Leu
  | Lys
  | Met
  | Phe
  | Pro
  | Ser
  | Thr
  | Trp
  | Tyr
  | Val

type protein = aminoacid list

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

(* rna -> (nucleobase * nucleobase * nucleobase) list. *)
let generate_bases_triplets (rna: rna) =
  match rna with
  | [] -> []
  | l ->
    let rec generate_bases_triplets_aux list acc =
      match list with
      | a::b::c::t -> generate_bases_triplets_aux t (acc @ [(a, b, c)])
      | _ -> acc in
    generate_bases_triplets_aux l []

let rec match_decode = function 
  | (U,A,A) | (U,A,G) | (U,G,A) -> (Stop, true)
  | (G,C,A) | (G,C,C) | (G,C,G) | (G,C,U) -> (Ala, true)
  | (A,G,A) | (A,G,G) | (C,G,A) | (C,G,C) | (C,G,G) | (C,G,U) -> (Arg, true)
  | (A,A,C) | (A,A,U) -> (Asn, true)
  | (G,A,C) | (G,A,U) -> (Asp, true)
  | (U,G,C) | (U,G,U) -> (Cys, true)
  | (C,A,A) | (C,A,G) -> (Gln, true)
  | (G,A,A) | (G,A,G) -> (Glu, true)
  | (G,G,A) | (G,G,C) | (G,G,G) | (G,G,U) -> (Gly, true)
  | (C,A,C) | (C,A,U) -> (His, true)
  | (A,U,A) | (A,U,C) | (A,U,U) -> (Ile, true)
  | (C,U,A) | (C,U,C) | (C,U,G) | (C,U,U) | (U,U,A) | (U,U,G) -> (Leu, true)
  | (A,A,A) | (A,A,G) -> (Lys, true)
  | (A,U,G) -> (Met, true)
  | (U,U,C) | (U,U,U) -> (Phe, true)
  | (C,C,C) | (C,C,A) | (C,C,G) | (C,C,U) -> (Pro, true)
  | (U,C,A) | (U,C,C) | (U,C,G) | (U,C,U) | (A,G,U) | (A,G,C) -> (Ser, true)
  | (A,C,A) | (A,C,C) | (A,C,G) | (A,C,U) -> (Thr, true)
  | (U,G,G) -> (Trp, true)
  | (U,A,C) | (U,A,U) -> (Tyr, true)
  | (G,U,A) | (G,U,C) | (G,U,G) | (G,U,U) -> (Val, true)
  | _ -> (Stop, false)

(* rna -> protein *)
let decode_arn rna: protein =
  match rna with
  | [] -> []
  | list ->
      let bases_triplets = generate_bases_triplets list in
      let rec decode_arn_aux list acc =
        match list with
        | h::t ->
          let (decode, success) = match_decode h in
          if success
          then if decode = Stop then acc @ [decode] else decode_arn_aux t (acc @ [decode])
          else acc
        | [] -> acc
      in
      decode_arn_aux bases_triplets []
      
(* Tests *)

let display_nucleobases = List.iter print_endline

let print_triplet = function
  | (a, b, c) ->
    print_char '(';
    print_char (nucleobase_to_char a);
    print_char (nucleobase_to_char b);
    print_char (nucleobase_to_char c);
    print_char ')'

let aminoacid_to_string = function
  | Stop -> "End of translation"
  | Ala -> "Alanine"
  | Arg -> "Arginine"
  | Asn -> "Asparagine"
  | Asp -> "Aspartique"
  | Cys -> "Cysteine"
  | Gln -> "Glutamine"
  | Glu -> "Glutamique"
  | Gly -> "Glycine"
  | His -> "Histidine"
  | Ile -> "Isoleucine"
  | Leu -> "Leucine"
  | Lys -> "Lysine"
  | Met -> "Methionine"
  | Phe -> "Phenylalanine"
  | Pro -> "Proline"
  | Ser -> "Serine"
  | Thr -> "Threonine"
  | Trp -> "Tryptophane"
  | Tyr -> "Tyrosine"
  | Val -> "Valine"

let helix_from_str = function
  | "" -> []
  | s -> let rec helix_from_str_aux s i acc =
    match i with
    | 0 -> acc
    | n -> helix_from_str_aux s (i - 1) (generate_nucleotide s.[i] :: acc ) in
  helix_from_str_aux s ((String.length s) - 1) [] 

let display_nucleotide nucleotide =
  print_string "phosphate: ";
  print_endline nucleotide.phosphate;
  print_string "deoxyribose: ";
  print_endline nucleotide.deoxyribose;
  print_string "nucleobase: ";
  print_string (nucleobase_to_string nucleotide.nucleobase)

let display_helix helix =
  let rec display_helix_aux h i =
    match h with
    | [] -> print_char '\n'
    | h::t ->
      print_string "Nucleotide ";
      print_int i;
      print_endline ":";
      display_nucleotide h;
      print_string "\n\n";
      display_helix_aux t (i + 1) in
  display_helix_aux helix 1

let display_rna rna =
  List.iter (fun e -> print_char (nucleobase_to_char e)) rna;
  print_char '\n'

let display_bases_triplets bases_triplets =
  List.iter (fun e -> print_triplet e) bases_triplets;
  print_char '\n'

let display_protein protein =
  print_string "[ ";
  let rec display_protein_aux = function
    | h::[] -> print_string (aminoacid_to_string h); display_protein_aux []
    | h::t -> print_string (aminoacid_to_string h); print_string ", "; display_protein_aux t
    | [] -> print_string " ]" in
  display_protein_aux protein

let life = function
  | "" -> ()
  | s ->
    print_string "About to create some life from the following sequence: ";
    print_endline s;
    print_char '\n';
    let helix = helix_from_str s in
    print_endline "Helix created:\n";
    display_helix helix;
    let rna = generate_rna helix in
    print_string "RNA created: ";
    display_rna rna;
    print_char '\n';
    let bases_triplets = generate_bases_triplets rna in
    print_string "Bases triplets: ";
    display_bases_triplets bases_triplets;
    print_char '\n';
    let protein = decode_arn rna in
    print_string "Protein Created: ";
    display_protein protein;
    print_char '\n'

(* let test n =
  print_string "Test with ";
  print_string "[";
  print_int n;
  print_endline "]: ";
  let helix = generate_helix n in
  print_string "rna: ";
  let rna = generate_rna helix in
  List.iter (fun e -> print_char (nucleobase_to_char e)) rna;
  print_string "\nbase triplets: ";
  print_char '[';
  let base_triplets = generate_bases_triplets rna in
  print_char ']';
  List.iter (fun e -> print_triplet e) base_triplets;
  print_char '\n';
  let protein = decode_arn rna in
  print_endline "Protein:";
  List.iter (fun e -> print_endline (aminoacid_to_string e)) protein *)

let main () =
  life "ATCG"
  (* test 5;
  test 18;
  test 150 *)
  
let () = main ()