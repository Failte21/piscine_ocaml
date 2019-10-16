let rec count_1 = function
  | [] -> 0
  | h::t -> h + count_1 t

let is_odd n = (n mod 2) = 0

let rec rev_last = function
  | [] -> []
  | h::[] -> if h = 1 then [0] else [1]
  | h::t -> h :: (rev_last t)

let rec last_1_index l i last =
  match l with
  | [] -> last
  | h::[] -> if h = 0 then last else i
  | h::t ->
    let last = if h = 1 then i else last in
    last_1_index t (i + 1) last

let rev_weird l =
  let i = (last_1_index l 0 0) - 1 in
  let rec loop l j =
    match l with
    | [] -> []
    | h::t when i = j ->
      let e = if h = 0 then 1 else 0 in
      e::t
    | h::t -> h::(loop t (j + 1)) in
  loop l 0

let rec check_end l i =
  match l with
  | [] -> true
  | h::t ->
    let expected = if (i = 0) then 1 else 0 in
    (h = expected) && (check_end t (i + 1))

let create_list n =
  if n <= 0 then [] else
  let rec create_list_aux = function
    | 0 -> []
    | i -> 0::(create_list_aux (i - 1)) in
  create_list_aux n

let gray n =
  let rec loop l =
    List.iter print_int l;
    print_char ' ';
    if (check_end l 0 = false) then
      let n_1 = count_1 l in
      let l = if is_odd n_1 then
        (rev_last l) else (rev_weird l) in
      loop l in
  loop(create_list n);
  print_char '\n'

let test n =
  print_string "Test with ";
  print_string "[";
  print_int n;
  print_string "]: ";
  gray n

let main () =
  test (-1);
  test 2;
  test 3;
  test 5

let () = main ()