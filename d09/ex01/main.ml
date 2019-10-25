let green = "\x1b[32m"
let reset = "\x1b[0m"

let (--) i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc) in
  aux j []

let print_prog (s, status, grade) =
  print_string "s: "; print_endline s;
  print_string "Status: "; print_endline status;
  print_string "Grade: "; print_int grade; print_char '\n'

let successful_program = ("Incredible Program", "success", 1500)
let unsuccessful_program = ("Disastrous Program", "fail", -1500)
let simply_average = ("Simply average", "success", 80)
let good_one = ("Good one", "success", 400)

let all_programs = [
  successful_program;
  unsuccessful_program;
  simply_average;
  good_one
]

let test_combine =
  Printf.printf "%s-----TEST COMBINE-----%s\n" green  reset;
  let final_program = List.fold_left
  (fun acc program ->
    begin
      print_char '\n';
      print_prog acc;
      print_endline "COMBINE WITH: "; print_prog program;
      App.App.combine acc program
    end
  ) App.App.zero all_programs in
  print_prog final_program;
  print_char '\n'

let test_success =
  Printf.printf "%s-----TEST SUCCESS-----%s\n" green  reset;
  print_prog unsuccessful_program;
  print_endline "------------>";
  print_prog (App.App.success unsuccessful_program);
  print_char '\n'

let test_fail =
  Printf.printf "%s-----TEST FAIL-----%s\n" green  reset;
  print_prog successful_program;
  print_endline "------------>";
  print_prog (App.App.fail successful_program);
  print_char '\n'

let test =
  test_combine;
  test_success;
  test_fail

let main () = test

let () = main ()