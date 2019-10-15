(* Allowed functions : Pervasives module. *)
(* val encode : ’a list -> (int * ’a) list *)

let encode l =
  let rec loop l e n =
    match l with
    | h::t when h = e -> loop t e (n + 1)
    | h::t -> (n, e) :: loop t h 1
    | [] -> [(n, e)]
  in
  match l with
  | h::t -> loop (h::t) h 0
  | [] -> []

let print_pair pfa pfb (a, b) =
  print_char '(';
  pfa a;
  print_string ", ";
  pfb b;
  print_char ')'

let print_pair_int_x = print_pair print_int
let print_pair_int_char = print_pair_int_x print_char
let print_pair_int_string = print_pair print_int print_string

let print_list pf l =
  print_char '[';
  let rec print_pair_list_aux l =
    match l with
    | [] -> ()
    | [t] -> (pf t);
    | h::t -> (pf h); print_char ' '; print_pair_list_aux t in
  print_pair_list_aux l;
  print_char ']'

let print_char_list = print_list print_char
let print_string_list = print_list print_string
let print_pair_int_char_list = print_list print_pair_int_char
let print_pair_int_string_list = print_list print_pair_int_string

let test pargf pf arg =
  print_string "Test with ";
  print_string "[";
  pargf arg;
  print_string "]: ";
  pf (encode arg);
  print_char '\n'

let test_char = test print_char_list print_pair_int_char_list
let test_string = test print_string_list print_pair_int_string_list

let main () =
  test_char ['a'; 'a'; 'a'; 'b'; 'b'];
  test_char ['a'; 'c'; 'd'; 'c'; 'b'];
  test_char [];
  test_string ["hello"; "hello"; "test"; "test"; "test"; "a"; ""];
  test_string ["hello"]
  
let () = main ()