(* If any argument given to the function is negative, the function must return -1. *)

let rec ackermann m n =
  if m < 0 || n < 0 then -1 else
  if m = 0 then (n + 1)
  else if m > 0 && n = 0 then (ackermann (m - 1) 1)
  else (ackermann (m - 1)) (ackermann m (n - 1))

let test m n =
  print_string "Test with ";
  print_string "[";
  print_int m;
  print_char ' ';
  print_int n;
  print_string "]: ";
  print_int (ackermann m n);
  print_char '\n'

let main () =
test (-1) 7;
(* -1 *)
test 0 0;
(* 1 *)
test 2 3;
(* 9 *)
test 4 1
(* 65533 *)
  
let () = main ()