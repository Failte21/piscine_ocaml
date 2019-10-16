(* Allowed functions : atan, float_of_int *)

(* If the given delta is negative, your
function will return -1 *)

(* Its type will be float -> int *)

(* float -> int *)
let leibniz_pi delta =
  if delta < 0.0 then -1 else
  let f = (fun n -> ((-1.0) ** (float_of_int n)) /. ((2.0 *. (float_of_int n)) +. 1.0)) in
  let pi_ref = float_of_int 4 *. atan 1.0 in
  let rec leibniz_pi_aux i acc =
    let result = (f i) +. acc in
    let diff = pi_ref -. (4.0 *. result) in
    let diff_abs = if diff > 0.0 then diff else -.diff in
    if diff_abs < delta then i
    else leibniz_pi_aux (i + 1) (result) in
  leibniz_pi_aux 0 0.0

let test delta =
  print_string "Test with ";
  print_string "[";
  print_float delta;
  print_string "]: ";
  print_int (leibniz_pi delta);
  print_char '\n'

let main () =
  test 1.0;
  test 0.1;
  test (-1.0);
  test 0.000001;
  test 0.000000001
  
let () = main ()