let green = "\x1b[32m"
let reset = "\x1b[0m"

let (--) i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc) in
  aux j []

let foo a_list b_list =
  let rec foo_aux a_list b_list acc =
    match a_list with
      | ha::ta ->
        begin
          match b_list with
          | hb::tb -> foo_aux ta tb acc @ [(ha, hb)]
          | [] -> acc
        end
      | [] -> acc in
    foo_aux a_list b_list []

let a_list = [1;4;12;150;-12;13;0]
let b_list = [4;0;12;8;12;-130;75]
let duets = foo a_list b_list

let test_op op op_sym =
  List.iter
  (fun (a, b) ->
    let hour_a: Watchtower.Watchtower.hour = a in
    let hour_b: Watchtower.Watchtower.hour = b in
    Printf.printf "%d %s %d: %d\n" a op_sym b (op hour_a hour_b)
  )
  duets

let test_add = test_op Watchtower.Watchtower.add "+"
let test_sub = test_op Watchtower.Watchtower.sub "-"


let main () =
  test_add

let () = main ()