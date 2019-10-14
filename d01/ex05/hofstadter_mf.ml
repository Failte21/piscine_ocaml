(* If given a negative argument, your functions must return -1. *)

(* Obviously, each sequence must be implemented only once. Implementing
them more than once means you have failed the exercise. *)

let rec hfs_f n =
  if n < 0 then -1 else
  if n = 0 then 1 else
  (n - (hfs_m (hfs_f (n - 1)) ))

and hfs_m n =
  if n < 0 then -1 else
  if n = 0 then 0 else
  (n - (hfs_f (hfs_m (n - 1))))

let get_f fname =
  if fname = "hfs_f" then hfs_f else hfs_m

let test fname n =
  let f = get_f fname in
  print_string "Test with ";
  print_string "[";
  print_string fname;
  print_char ' ';
  print_int n;
  print_string "]: ";
  print_int (f n);
  print_char '\n'

let main () =
  test "hfs_m" 0;
  (* 0 *)
  test "hfs_f" 0;
  (* 1 *)
  test "hfs_m" 4;
  (* 2 *)
  test "hfs_f" 4;
  (* 3 *)
  test "hfs_f" (-1);
  (* 3 *)
  test "hfs_f" (-1)
  (* 3 *)
  
let () = main ()