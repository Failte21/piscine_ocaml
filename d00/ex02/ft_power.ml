(* Both parameters will always be positives or equal to 0, but
both will never be equal to 0 at the same time. *)

let rec ft_power a b =
  if b = 0 then
    1
  else
    a * (ft_power a (b - 1))

let test a b =
  print_string "test with [";
  print_int a;
  print_char '^';
  print_int b;
  print_string "]: ";
  print_int (ft_power a b);
  print_char '\n'

let main () =
  test 2 2;
  test 3 1;
  test 0 2;
  test 2 0;
  test 3 3

let () = main ()