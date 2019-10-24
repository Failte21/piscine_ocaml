(* Allowed functions : Pervasives modules *)

class people name =
  object
    initializer print_endline "Would you like a jelly baby?"
    val name = name
    val hp = 100
    method to_string = name
    method talk = print_endline ("I’m " ^ name ^ "! Do you know the Doctor?")
    method die = print_endline "Aaaarghh!"
  end