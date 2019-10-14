(* Allowed functions : print_int and print_string *)

let ft_print_comb () =
  let rec loop a b c =
    print_int a;
    print_int b;
    print_int c;
    if a < 7 then
      begin
        print_string ", ";
        let new_a = if b = 8 then a + 1 else a in
        let new_b = if b = 8 then new_a + 1 else if c == 9 then b + 1 else b in
        let new_c = if c = 9 then new_b + 1 else c + 1 in
        loop new_a new_b new_c
      end
    in
  loop 0 1 2;
  print_string "\n"

let main () = ft_print_comb()
  
let () = main ()