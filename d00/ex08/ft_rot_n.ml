(* Allowed functions: char_of_int, int_of_char, String.map *)
(* Input: n always positive *)

let ascii_rot n c a_int =
  let c_int = int_of_char c in
  let base = c_int - a_int + n in
  let result = base mod 26 in
  char_of_int (a_int + result)

let rot n c =
  let a_low_int = int_of_char 'a' in
  let z_low_int = int_of_char 'z' in
  let a_cap_int = int_of_char 'A' in
  let z_cap_int = int_of_char 'Z' in
  let c_int = int_of_char c in
  let a_int =
    if c_int >= a_low_int && c_int <= z_low_int then a_low_int
    else if c_int >= a_cap_int && c_int <= z_cap_int then a_cap_int
    else 0 in
  if a_int = 0 then c else ascii_rot n c a_int  

let ft_rot_n n str =
  String.map (rot n) str

let test i s =
  print_string "Test with [";
  print_int i;
  print_char ' ';
  print_string s;
  print_string "]: ";
  print_endline (ft_rot_n i s)

let main () =
  test 1 "abcdefghijklmnopqrstuvwxyz";
  test 42 "123456789";
  test 75 "hello";
  test 75 "Yolo";
  test 1 "NBzlk qnbjr !";
  test 0 "Damned !"
  
let () = main ()