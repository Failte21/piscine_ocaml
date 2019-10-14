let rec converges f x n =
  if n < 0 then false else
  let res = f x in
  if res = x then true else
  converges f res (n - 1)

let test f x n =
  print_string "Test with ";
  print_string "[";
  print_int x;
  print_char ' ';
  print_int n;
  print_string "]: ";
  if converges f x n
  then print_endline "True"
  else print_endline "False"

let main () =
  print_endline "With (( * ) 2)";
  test (( * ) 2) 2 3;
  test (( * ) 2) 2 (-1);
  print_endline "\nWith (fun x -> x / 2)";
  test (fun x -> x / 2) 2 3;
  print_endline "\n With (fun x -> x / 2)";
  test (fun x -> x / 2) 2 2;
  test (fun x -> x / 2) 2 0

let () = main ()