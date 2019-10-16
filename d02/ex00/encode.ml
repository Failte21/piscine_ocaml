(* Allowed functions : Pervasives module. *)
(* val encode : ’a list -> (int * ’a) list *)

let encode l =
  let rec encode_aux list element n acc =
    match list with
    | [] -> acc
    | h::[] when h = element -> (acc @ [(n + 1, element)]) 
    | h::[] -> acc @ [(1, element)] 
    | h::t when h = element -> encode_aux t element (n + 1) acc
    | h::t -> encode_aux t h 1 (acc @ [(n, element)])
  in
  (* let rec encode_aux list element n acc =
    match list with
    | [] -> acc
    | h::[] when h = element -> (n + 1, element) :: acc 
    | h::[] -> (1, element) :: acc
    | h::t when h = element -> encode_aux t element (n + 1) acc
    | h::t -> encode_aux t h 1 ((n, element) :: acc)
  in *)
  match l with
  | [] -> []
  | h::t -> encode_aux (h::t) h 0 []

let print_pair pfa pfb (a, b) =
  print_char '(';
  pfa a;
  print_string ", ";
  pfb b;
  print_char ')'

let print_pair_int_string = print_pair print_int print_string
let print_pair_int_char = print_pair print_int print_char

let print_string_list = List.iter (fun e -> print_char ' '; print_string e;)
let print_pair_int_char_list = List.iter print_pair_int_char
let print_pair_int_string_list = List.iter print_pair_int_string

let test pargf pf arg =
  print_string "Test with ";
  print_string "[";
  pargf arg;
  print_string "]: ";
  pf (encode arg);
  print_char '\n'

let test_char = test (List.iter print_char) print_pair_int_char_list
let test_string = test print_string_list print_pair_int_string_list

let main () =
  test_char ['a'; 'a'; 'a'; 'b'; 'b'];
  test_char ['a'; 'c'; 'd'; 'c'; 'b'];
  test_char [];
  test_string ["hello"; "hello"; "test"; "test"; "test"; "a"; ""];
  test_string ["hello"]
  
let () = main ()