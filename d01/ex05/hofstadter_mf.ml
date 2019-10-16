(* If given a negative argument, your functions must return -1. *)

(* Obviously, each sequence must be implemented only once. Implementing
   them more than once means you have failed the exercise. *)

(* int -> int *)
let rec hfs_f n =
  if n < 0 then -1 else
  if n = 0 then 1 else
    (n - (hfs_m (hfs_f (n - 1)) ))

(* int -> int *)
and hfs_m n =
  if n < 0 then -1 else
  if n = 0 then 0 else
    (n - (hfs_f (hfs_m (n - 1))))

let get_f fname =
  if fname = "hfs_f" then hfs_f else hfs_m

let max = 20

let test f n =
  print_int (f n);
  if n < max then print_string ", " else print_char '\n'

let (--) i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc) in
  aux j []

let main () =
  print_endline "The first few terms of the \"male\" sequence:";
  List.iter (test hfs_m) (-5--max);
  print_char '\n';
  print_endline "The first few terms of the \"female\" sequence:";
  List.iter (test hfs_f) (-5--max)

let () = main ()