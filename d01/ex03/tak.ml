let rec tak x y z =
  if y >= x then
    z
  else
    tak (tak (x - 1) y z) (tak (y - 1) z x) (tak (z - 1) x y)

let test x y z =
  print_string "Test with ";
  print_string "[";
  print_int x;
  print_char ' ';
  print_int y;
  print_char ' ';
  print_int z;
  print_string "]: ";
  print_int (tak x y z);
  print_char '\n'

let main () =
  test 1 2 3;
  test 5 23 7;
  test 9 1 0;
  test 1 1 1;
  test 0 42 0;
  test 23498 98734 98776

let () = main ()