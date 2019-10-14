(* Allowed functions : float_of_int *)
(* If the upper bound is less than the lower bound, ft_sum must return nan. *)

let ft_sum f i upper_bound =
  if upper_bound < i then nan else
  let rec ft_sum_aux f i upper_bound acc =
    if upper_bound = i then acc else
    ft_sum_aux f (i + 1) upper_bound (f (i + 1) +. acc) in
  ft_sum_aux f i upper_bound (f i)

let main () = print_float (ft_sum (fun i -> float_of_int (i * i)) 1 10)
  
let () = main ()