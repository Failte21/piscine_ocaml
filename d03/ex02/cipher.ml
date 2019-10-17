(* Allowed functions : Pervasives module and the String.map function *)

let rot n c =
  let c_int = int_of_char c in
  char_of_int ((c_int + n) mod 128)

let ceasar s i = String.map (rot i) s

let rot42 s = ceasar s 1

let xorc key c = char_of_int (((int_of_char c) lxor key) mod 128)

let xor key s = String.map (xorc key) s

let ft_crypt (s: string) fns =
  let rec ft_crypt_aux fns acc =
    match fns with
    | fn::t -> ft_crypt_aux t (fn s)
    | [] -> acc in
  ft_crypt_aux fns s