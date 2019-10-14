(* Allowed functions : char_of_int, int_of_char and print_char *)

let ft_print_alphabet () =
  let rec loop letter =
    print_char letter;
    if letter != 'z' then
      begin
        let int_letter = int_of_char letter in
        let next_letter = char_of_int (int_letter + 1) in
        loop next_letter
      end
    in
    loop 'a';
    print_char '\n'

let main () = ft_print_alphabet()
  
let () = main ()