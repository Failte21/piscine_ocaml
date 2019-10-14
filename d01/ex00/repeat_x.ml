(* If the argument given to the function is negative, the function must return "Error". *)

let rec repeat_x n =
  if n < 0 then "Error" else
    if n == 0 then
      ""
    else
      "x" ^ (repeat_x (n - 1))

let test n =
  print_string "Test with ";
  print_string "[";
  print_int n;
  print_string "]: ";
  let result = repeat_x n in
  print_string result;
  print_string ", len: ";
  print_int (String.length result);
  print_char '\n'

let main () =
  test 5;
  test (-1);
  test 12;
  test 500;
  test 0
  
let () = main ()