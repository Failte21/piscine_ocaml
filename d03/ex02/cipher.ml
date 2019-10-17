(* Allowed functions : Pervasives module and the String.map function *)

let rot n c =
  let c_int = int_of_char c in
  char_of_int ((c_int + n) mod 128)

let ceasar s i = String.map (rot i) s

let unceasar s i = String.map (rot ((128 - i))) s

let rot42 s = ceasar s 42

let unrot42 s = unceasar s 42

let xorc key c = char_of_int ((int_of_char c) lxor key)

let xor key s = String.map (xorc key) s

let ft_crypt (s: string) fns =
  let rec ft_crypt_aux fns acc =
    match fns with
    | fn::t -> ft_crypt_aux t (fn s)
    | [] -> acc in
  ft_crypt_aux fns ""

let main () =
  let rot_hello = ceasar "hello le zebre 1234" 1 in
  print_endline rot_hello;
  print_endline (unceasar rot_hello 1);
  let xored = xor 5 "hello" in
  print_endline xored;
  print_endline (xor 5 xored);
  print_endline "----ABOUT TO ENCRYPT [hello]----";
  let crypted = ft_crypt "hello" [rot42; (xor 18)] in
  print_endline crypted;
  print_endline (ft_crypt crypted [rot42; (xor 18)])

let () = main ()