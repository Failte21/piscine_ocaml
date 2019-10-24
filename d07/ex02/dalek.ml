(* Allowed functions : Pervasives, String and Random modules *)

let random_char () =
  let int_a = int_of_char 'a' in
  let int_z = int_of_char 'z' in
  let int_A = int_of_char 'A' in
  let int_Z = int_of_char 'Z' in
  let lc = char_of_int ((Random.int (int_z - int_a)) + int_a) in
  let uc = char_of_int ((Random.int (int_Z - int_A)) + int_A) in
  if Random.bool () then lc else uc

let random_catchphrase () =
  let i = Random.int (4) in
  match i with
    | 0 -> "Explain! Explain!"
    | 1 -> "Exterminate! Exterminate!"
    | 2 -> "You are the Doctor! You are the enemy of the Daleks!"
    | _ -> "I obey!"

let rec random_name index acc =
  if (index > 0) then
    begin
      let c = random_char () in
      random_name (index - 1) (acc ^ (String.make 1 c))
    end
  else acc

class dalek =
  object
    val name = random_name 3 "Dalek"
    val hp = 100
    val mutable shield: bool = true
    method to_string = "name: " ^ name ^ ", hp: " ^ string_of_int hp ^ ", shield: " ^ string_of_bool shield
    method talk = print_endline (random_catchphrase ())
    method exterminate (people: People.people) =
      people#die;
      shield <- not shield;
  end