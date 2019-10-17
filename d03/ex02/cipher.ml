(* Allowed functions : Pervasives module and the String.map function *)

let ascii_rot n c a_int =
  let c_int = int_of_char c in
  let base = c_int - a_int + n in
  let result = base mod 26 in
  char_of_int (a_int + result)

let rot n c =
  match c with
  | 'a' .. 'z' -> ascii_rot n c (int_of_char 'a')
  | 'A' .. 'Z' -> ascii_rot n c (int_of_char 'A')
  | _ -> c

let ceasar s i = String.map (rot i) s

let unceasar s i = String.map (rot (26 - (i mod 26))) s

let rot42 s = ceasar s 42

let unrot42 s = unceasar s 42

let main () =
  let rot_hello = rot42 "hello le zebre 1234" in
  print_endline rot_hello;
  print_endline (unrot42 rot_hello)

let () = main ()