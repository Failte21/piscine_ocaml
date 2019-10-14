(* If n is negative, your function will return -1 *)

let rec iter f x n =
  if n < 0 then (-1) else
  if n = 0 then x else
  if n = 1 then (f x) else
  iter f (f x) (n - 1)

let test f x n =
  print_string "Test with ";
  print_string "[";
  print_int x;
  print_char ' ';
  print_int n;
  print_string "]: ";
  print_int (iter f x n);
  print_char '\n'

let main () =
  print_endline "With (fun x -> x * x)";
  test (fun x -> x * x) 2 4;
  test (fun x -> x * x) 1 100;
  test (fun x -> x * x) 3 0;
  test (fun x -> x * x) 100 (-1);
  test (fun x -> x * x) (-3) 3;
  print_endline "\nWith (fun x -> x * 2)";
  test (fun x -> x * 2) 2 4;
  test (fun x -> x * 2) 5 3;
  test (fun x -> x * 2) 5 0;
  test (fun x -> x * 2) 5 (-1);
  test (fun x -> x * 2) (-5) 2
  
let () = main ()