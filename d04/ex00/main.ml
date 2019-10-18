let green = "\x1b[32m"
let reset = "\x1b[0m"

let main () =
  let all_colors = Color.all in
  Printf.printf "%s-----Color.toString-----%s\n" green reset;
  List.iter print_endline (List.map Color.toString all_colors);
  Printf.printf "%s-----Color.toStringVerbose-----%s\n" green reset;
  List.iter print_endline (List.map Color.toStringVerbose all_colors)

let () = main ()