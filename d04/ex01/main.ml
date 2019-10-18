let green = "\x1b[32m"
let reset = "\x1b[0m"

let next_str v =
  (Value.toString v) ^ "->" ^ (Value.toString (Value.next v))


let previous_str v =
  (Value.toString v) ^ "->" ^ (Value.toString (Value.previous v))

let main () =
  let all_values = Value.all in
  Printf.printf "%s-----Value.toString-----%s\n" green reset;
  List.iter print_endline (List.map Value.toString Value.all);
  print_char '\n';
  Printf.printf "%s-----Value.toStringVerbose-----%s\n" green reset;
  List.iter print_endline (List.map Value.toStringVerbose all_values);
  print_char '\n';
  Printf.printf "%s-----Value.next-----%s\n" green reset;
  List.iter print_endline (List.map next_str Value.without_as);
  print_char '\n';
  Printf.printf "%s-----Value.previous-----%s\n" green reset;
  List.iter print_endline (List.map previous_str Value.without_2);
  print_char '\n';
  Printf.printf "%s-----Value.next As-----%s\n" green reset;
  List.iter print_endline (List.map next_str Value.all);
  print_char '\n';
  Printf.printf "%s-----Value.previous 2-----%s\n" green reset;
  List.iter print_endline (List.map previous_str Value.all)

let () = main ()