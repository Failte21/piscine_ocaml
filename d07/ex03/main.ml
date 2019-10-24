let green = "\x1b[32m"
let reset = "\x1b[0m"

let (--) i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc) in
  aux j []

let james = new People.people "James"
let peoples = [
  (new People.people "John");
  (new People.people "Carry");
  (new People.people "Lydia")
]

let watson = new Doctor.doctor "Watson" 100 james
let doctors = [
  new Doctor.doctor "Who" 100 (List.nth peoples 0);
  new Doctor.doctor "Calligari" 68 (List.nth peoples 1);
  new Doctor.doctor "Folamour" 120 (List.nth peoples 2)
]

let dalek = new Dalek.dalek
let daleks = List.map (fun _ -> new Dalek.dalek) (0--3)

let people_army = new Army.army peoples
let doctor_army = new Army.army doctors
let dalek_army = new Army.army daleks

let test_army army new_elem =
  Printf.printf "%s-----Initial Army-----%s\n" green  reset;
  List.iter (fun e -> print_endline e#to_string) army#get_member;
  Printf.printf "%s-----With a new element-----%s\n" green  reset;
  let new_army = army#add new_elem in
  List.iter (fun e -> print_endline e#to_string) new_army#get_member;
  Printf.printf "%s-----With some loss-----%s\n" green reset;
  let loss_army = new_army#delete in
  List.iter (fun e -> print_endline e#to_string) loss_army#get_member;
  print_char '\n'

let test_people_army = test_army people_army james
let test_doctor_army = test_army doctor_army watson
let test_dalek_army = test_army dalek_army dalek

let test_armies =
  test_people_army;
  test_doctor_army;
  test_dalek_army

let rec test_delete_forever army =
  print_endline "LOSING SOMEONE....";
  test_delete_forever army#delete

let test =
  test_armies;
  test_delete_forever doctor_army

let main () = test

let () = main ()