let rec ft_countdown number =
  if number <= 0
  then
    begin
      print_int 0;
      print_char '\n'
    end
  else
    begin
      print_int number;
      print_char '\n';
      ft_countdown (number - 1)
    end

let test arg =
  begin
    print_string "test with [";
    print_int arg;
    print_endline "]:";
    ft_countdown arg
  end

let main () =
  test 15;
  test 5;
  test 0;
  test (-15)

let () = main ()