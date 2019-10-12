(* Allowed functions: char_of_int, int_of_char, String.map *)
(* Input: n always positive *)

let ascii_rot n c =
  let z_int = int_of_char 'z' in
  let c_int = int_of_char c in
  if c_int + n <= z_int then
    char_of_int (c_int + n)
  else
    let e = z_int - c_int - 1 in
    char_of_int (c_int + e)

let ft_rot_n n str =
  let z_int = int_of_char 'z' in
  let a_int = int_of_char 'a' in
  let size = z_int - a_int in
  let safe_n = if n > size then 0 else n in
  String.map (ascii_rot safe_n) str

let test i s =
  print_string "Test \"";
  print_string s;
  print_string "\": ";
  print_endline (ft_rot_n i s)

let main () =
  test 1 "abcdefghijklmnopqrstuvwxyz";
  test 42 "123456789"
  
let () = main ()