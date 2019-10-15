(* Allowed functions : print_endline *)

let ft_test_sign number =
  if number >= 0
  then print_endline "positive"
  else print_endline "negative"

let test number =
  begin
    print_string "test with [";
    print_int number;
    print_string "]: ";
    ft_test_sign number;
  end

let main () =
  test 42;
  test 0;
  test (-1);
  test (-100);
  test (-(-5))

let () = main ()