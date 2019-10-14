(* Allowed functions : print_int and print_string *)

let ft_print_comb () =
  let rec loop a b c =
    print_int a;
    print_int b;
    print_int c;
    if a < 7 then
      begin
        print_string ", ";
        if c = 9 then
          if b = (c - 1) then
            begin
              let next_a = a + 1 in
              loop next_a b c;
            end
          else
            let next_b = b + 1 in
            let next_c = next_b + 1 in
            loop a next_b next_c
        else
          let next_c = c + 1 in
          loop a b next_c
      end
    in
  loop 0 1 2;
  print_string "\n"

let main () = ft_print_comb()
  
let () = main ()