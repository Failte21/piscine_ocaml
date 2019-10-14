(* Allowed functions : print_char, print_int *)

let print_nb nb =
  if nb < 10 then
    print_int 0;
  print_int nb

let ft_print_comb2 () =
  let max = 99 in
  let rec loop a b =
    print_nb a;
    print_char ' ';
    print_nb b;
    if a < (max - 1) then
      begin
        print_char ',';
        print_char ' ';
        let new_a = if b = max then a + 1 else a in
        let new_b = if b = max then new_a + 1 else b + 1 in
        loop new_a new_b
      end
    in
    loop 0 1;
    print_char '\n'
    
let main () = ft_print_comb2 ()
  
let () = main ()