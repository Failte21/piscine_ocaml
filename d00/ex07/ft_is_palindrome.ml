(* Allowed functions: String.get, String.length *)

let is_eq s i =
  let a = String.get s i in
  let b = String.get s ((String.length s) - i - 1) in
  a = b

let ft_is_palindrome s =
  let rec loop s i =
    if i = 0 then
      is_eq s i
    else
      is_eq s i && loop s (i - 1) in
  let len = String.length s in
  if len = 0 then true
  else loop s (len - 1)

let test s =
  print_string "Test \"";
  print_string s;
  print_string "\": ";
  if ft_is_palindrome s then
    print_string "True\n"
  else
    print_string "False\n"

let main () =
  test "radar";
  test "car";
  test "madam";
  test ""
  
let () = main ()