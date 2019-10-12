let ft_power a b =
  if b = 0 then
    1
  else
    begin
      if (b = 1)
        then a
      else
        begin
          let rec loop x y =
            if y > 1 then
              (x * x) + (loop x (y - 1))
            else
              0
          in
          loop a b
        end
    end


let test a b =
  print_string "[test ";
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
  test 2 0

let () = main ()