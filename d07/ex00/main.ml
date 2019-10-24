let main () =
  let john = new People.people "John" in
  print_endline john#to_string;
  john#talk;
  john#die

let () = main ()