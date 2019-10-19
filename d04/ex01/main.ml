let green = "\x1b[32m"
let reset = "\x1b[0m"

let next_str v =
  (Value.toString v) ^ "->" ^ (Value.toString (Value.next v))


let previous_str v =
  (Value.toString v) ^ "->" ^ (Value.toString (Value.previous v))

let main () =
  let without_2 = List.tl Value.all in
  let without_as = List.rev (List.tl (List.rev Value.all)) in
  let t_2 = List.hd Value.all in
  let t_as = List.hd (List.rev Value.all) in
  Printf.printf "%s-----Value.toString-----%s\n" green reset;
  List.iter print_endline (List.map Value.toString Value.all);
  print_char '\n';
  Printf.printf "%s-----Value.toStringVerbose-----%s\n" green reset;
  List.iter print_endline (List.map Value.toStringVerbose Value.all);
  print_char '\n';
  Printf.printf "%s-----Value.next (without AS)-----%s\n" green reset;
  List.iter print_endline (List.map next_str without_as);
  print_char '\n';
  Printf.printf "%s-----Value.previous (without 2)-----%s\n" green reset;
  List.iter print_endline (List.map previous_str without_2);
  print_char '\n';
  Printf.printf "%s-----Value.next AS-----%s\n" green reset;
  print_endline (next_str t_as);
  print_char '\n';
  Printf.printf "%s-----Value.previous 2-----%s\n" green reset;
  print_endline (previous_str t_2)

let () = main ()