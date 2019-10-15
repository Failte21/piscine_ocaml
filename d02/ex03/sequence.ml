(* val sequence : int -> string. *)
(* In case of an invalid parameter, the function should return an empty string. *)
(* Allowed functions : Pervasives module *)

let rec arr_to_str arr =
  match arr with
  | [] -> ""
  | h::t -> string_of_int h ^ arr_to_str t

let encode l =
  let rec loop l e n =
    match l with
    | [] -> [n; e]
    | h::t when h = e -> loop t e (n + 1)
    | h::t -> [n; e] @ loop t h 1
  in
  match l with
  | [] -> []
  | h::t -> loop (h::t) h 0

let sequence n =
  if n <= 0 then "" else
  if n = 1 then "1" else
  let rec loop l n =
    if n = 0 then l
    else loop (encode l) (n - 1) in
  arr_to_str (loop [1] (n - 1))

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