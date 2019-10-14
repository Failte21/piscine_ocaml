(* If the argument given to the function is negative, the function must behave like
repeat_x as stated in the previous exercise. *)

let rec repeat_string ?(str="x") n =
  if n < 0 then "Error" else
    if n == 0 then
      ""
    else
      str ^ (repeat_string ~str:str (n - 1))


let test ?(str="x") n =
  print_string "Test with ";
  print_string "[";
  if str != "x" then print_string str; print_char ' ';
  print_int n;
  print_string "]: ";
  if str = "x" then print_endline (repeat_string n)
  else print_endline (repeat_string ~str:str n)

let main () =
  test ~str:"hello " 3;
  test 5
  
let () = main ()