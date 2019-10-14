let rec fibonacci n =
  if n < 0 then (-1)
  else if n = 0 then 0
  else if n = 1 then 1 else (fibonacci (n - 2) + fibonacci (n - 1))

let test n =
  print_string "Test with ";
  print_string "[";
  print_int n;
  print_string "]: ";
  print_int (fibonacci n);
  print_char '\n'

let main () =
  test (-42);
  test 1;
  test 2;
  test 6
  
let () = main ()