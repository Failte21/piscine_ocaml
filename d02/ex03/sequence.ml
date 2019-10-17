(* val sequence : int -> string. *)
(* In case of an invalid parameter, the function should return an empty string. *)
(* Allowed functions : Pervasives module *)

let rec arr_to_str arr =
  match arr with
  | [] -> ""
  | h::t -> string_of_int h ^ (arr_to_str t)

let encode l =
  let rec encode_aux l e n acc =
    match l with
    | h::t when h = e -> encode_aux t e (n + 1) acc
    | h::t -> encode_aux t h 1 (acc @ [n; e])
    | [] -> acc @ [n; e]
  in
  match l with
  | [] -> []
  | h::t -> encode_aux (h::t) h 0 []

let sequence n =
  if n <= 0 then "" else
  if n = 1 then "1" else
  let rec sequence_aux acc n =
    if n = 0 then acc
    else (sequence_aux (encode acc) (n - 1)) in
  arr_to_str (sequence_aux [1] (n - 1))

let test n =
  print_string "Test with ";
  print_string "[";
  print_int n;
  print_string "]: ";
  print_endline (sequence n)

let (--) i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc) in
  aux j []

let main () =
  List.iter test (-3--15)
  
let () = main ()