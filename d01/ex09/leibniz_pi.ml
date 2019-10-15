(* Allowed functions : atan, float_of_int *)

(* If the given delta is negative, your
function will return -1 *)

let leibniz_pi delta =
  if delta < 0.0 then -1 else
  let f = (fun n -> ((-1.0) ** (float_of_int n)) /. ((2.0 *. (float_of_int n)) +. 1.0)) in
  let pi = float_of_int 4 *. atan 1.0 in
  let rec loop i acc =
    print_float (4.0 *. acc);
    print_char '\n';
    let result = (f i) +. acc in
    let diff = pi -. (4.0 *. result) in
    let diff_abs = if diff > 0.0 then diff else -.diff in
    if diff_abs < delta then i
    else loop (i + 1) (result) in
  loop 0 0.0

let main () = print_int (leibniz_pi 0.001)
  
let () = main ()