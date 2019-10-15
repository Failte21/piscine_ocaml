(* Allowed functions: String.get, String.length *)

let ft_string_all predicate s =
  let rec loop predicate s i =
    if i = 0 then
      predicate (String.get s i)
    else
      predicate (String.get s i) && (loop predicate s (i - 1))
  in
  let len = String.length s in
  if len = 0 then
    false
  else
    loop predicate s (len - 1)

let is_digit c =
  c >= '0' && c <= '9'

let test s =
  print_string "Test with [";
  print_string s;
  print_string "]: ";
  if ft_string_all is_digit s then
    print_string "True\n"
  else
    print_string "False\n"


let main () =
  print_endline "Tests with is_digit:\n";
  test "Hello";
  test "1234";
  test "Hello 1234";
  test "1234 Hello";
  test "   987";
  test ""

let () = main ()