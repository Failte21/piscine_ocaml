(* Allowed functions : Pervasives module. *)
(* val encode : ’a list -> (int * ’a) list *)

let encode l =
  let rec loop l e n =
    match l with
    | h::t when h == e -> loop t e (n + 1)
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

let rec print_list l =
  match l with
  | [] -> ()
  | [t] -> (print_pair_int_char t); print_char '\n';
  | h::t -> (print_pair_int_char h); print_char ' '; print_list t

let main () =
  let l = ['a'; 'a'; 'a'; 'b'; 'b'] in
  let result = (encode l) in
  print_list result
  
let () = main ()