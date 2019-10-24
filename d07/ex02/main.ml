let green = "\x1b[32m"
let reset = "\x1b[0m"

let (--) i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc) in
  aux j []

let peoples = [
  (new People.people "John");
  (new People.people "Carry");
  (new People.people "Lydia")
]

let kill_one (dalek: Dalek.dalek) people =
  print_endline dalek#to_string;
  people#talk;
  dalek#talk;
  dalek#exterminate people;
  print_char '\n'

let kill_them_all (dalek: Dalek.dalek) =
  List.iter(fun p -> kill_one dalek p) peoples;
  print_char '\n'

let test_dalek i =
  Printf.printf "%s-----TEST %d-----%s\n" green i reset ;
  let dalek = new Dalek.dalek in
  kill_them_all dalek

let rec test = function
  | i::t -> test_dalek i; test t
  | [] -> ()

let main () = test (1--10)

let () = main ()