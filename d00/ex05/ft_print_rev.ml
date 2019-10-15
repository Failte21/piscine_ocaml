(* Allowed functions: print_char, String.get, String.length *)

let ft_print_rev s =
  let rec loop s i =
    print_char (String.get s i);
    if i > 0 then
      loop s (i - 1)
  in
  let len = String.length s in
  if len > 0 then
    loop s (String.length(s) - 1);
  print_char '\n'

let test s =
  print_string "Test [";
  print_string s;
  print_string "]: ";
  ft_print_rev s

let main () =
  test "Hello, world!";
  test "Open the gates!";
  test "Lorem ipsum dolor sit amet, consectetur adipiscing elit.";
  test ""
  
let () = main ()