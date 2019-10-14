(* Allowed functions : float_of_int *)
(* If the upper bound is less than the lower bound, ft_sum must return nan. *)

let rec ft_sum f i upper_bound =
  if upper_bound < i then nan else
  if upper_bound = i then f i else
  f i +. (ft_sum f (i + 1) upper_bound)

let main () = print_float (ft_sum (fun i -> float_of_int (i * i)) 1 10)
  
let () = main ()