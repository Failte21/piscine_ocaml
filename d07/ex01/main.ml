let main () =
  let john = new People.people "John" in
  let who = new Doctor.doctor "Who" 100 john in
  print_endline who#to_string;
  let younger_who = who#travel_in_time 2019 1950 in
  print_endline younger_who#to_string

let () = main ()