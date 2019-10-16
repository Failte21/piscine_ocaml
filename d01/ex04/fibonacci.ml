(* int -> int *)
let fibonacci n =
  if n < 0 then (-1) else
    let rec fi_aux x a b =
      if x = 0 then a
      else if x = 1 then b
      else fi_aux (x - 1) b (a + b) in
    fi_aux n 0 1

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
  test 6;
  test 0;
  test 50

let () = main ()