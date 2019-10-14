(* Allowed functions : float_of_int *)
(* If the upper bound is less than the lower bound, ft_sum must return nan. *)

let ft_sum f i upper_bound =
  if upper_bound < i then nan else
  let rec ft_sum_aux f i upper_bound acc =
    if upper_bound = i then acc else
    ft_sum_aux f (i + 1) upper_bound (f (i + 1) +. acc) in
  ft_sum_aux f i upper_bound (f i)


let test f x n =
  print_string "Test with ";
  print_string "[";
  print_int x;
  print_char ' ';
  print_int n;
  print_string "]: ";
  print_float (ft_sum f x n);
  print_char '\n'

let main () =
  print_endline "With (fun float_of_int(x -> x * x))";
  test (fun x -> float_of_int(x * x)) 1 10;
  test (fun x -> float_of_int(x * x)) 1 100;
  test (fun x -> float_of_int(x * x)) 3 0;
  test (fun x -> float_of_int(x * x)) 100 (-1);
  test (fun x -> float_of_int(x * x)) (-3) 3;
  print_endline "\nWith (fun float_of_int(x -> x * 2))";
  test (fun x -> float_of_int(x * 2)) 2 4;
  test (fun x -> float_of_int(x * 2)) 5 3;
  test (fun x -> float_of_int(x * 2)) 5 0;
  test (fun x -> float_of_int(x * 2)) 5 (-1);
  test (fun x -> float_of_int(x * 2)) (-5) 2

let () = main ()