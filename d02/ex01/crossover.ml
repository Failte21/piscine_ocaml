(* val crossover : ’a list -> ’a list -> ’a list *)
(* We don’t have to handle duplicates in lists. *)

let rec crossover la lb =
  let rec cmp el l =
    match l with
    | [] -> []
    | h::t when h = el -> [el]
    | h::t -> cmp el t in
  match la with
  | [] -> []
  | h::t -> (cmp h lb) @ (crossover t lb)

let print_list pf l =
  print_char '[';
  let rec print_pair_list_aux l =
    match l with
    | [] -> ()
    | [t] -> (pf t);
    | h::t -> (pf h); print_char ' '; print_pair_list_aux t in
  print_pair_list_aux l;
  print_char ']'

let test pf la lb =
  print_string "Test with ";
  print_string "[";
  pf la;
  print_string ", ";
  pf lb;
  print_string "]: ";
  pf (crossover la lb);
  print_char '\n'

let print_string_list = print_list print_string
let test_string = test print_string_list

let main () =
  test_string ["test"; "hello"] ["test"; "yolo"];
  test_string ["open"; "open"; "yop"; "yala"; "the"; "cat"; "gate"] ["open"; "the"; "gate"];
  test_string ["open"; "open"; "yop"; "yala"; "the"; "cat"; "gate"] [];
  test_string [] []
  
let () = main ()