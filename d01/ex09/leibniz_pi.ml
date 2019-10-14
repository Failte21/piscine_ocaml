(* Allowed functions : atan, float_of_int *)

(* If the given delta is negative, your
function will return -1 *)

let leibniz_pi delta =
  if delta < 0.0 then -1 else
  let f = (fun n -> ((float_of_int (-1)) ** (float_of_int n)) /. (float_of_int 2 *. float_of_int n +. 1.0)) in
  let pi = float_of_int 4 *. atan 1.0 in
  let rec loop i delta acc =
    let result = (f i) +. acc in
    let diff = pi -. (4.0 *. result) in
    let diff_abs = if diff > 0.0 then diff else -.diff in
    if diff_abs < delta then i
    else loop (i + 1) delta (result) in
  loop 1 delta 0.0

let main () = print_int (leibniz_pi 3.14)
  
let () = main ()