(* Write a class doctor that has the following attributes :
◦ A name attribute of type string.
◦ An age attribute of type int.
◦ A sidekick attribute of type people
◦ An hp attribute of type int initialized to 100.
◦ A to_string method that returns the name of the object with attributes values.
◦ A talk method that print the following string on the standard output :
Hi! I’m the Doctor!
◦ An initializer which indicate that the object has been created (feel free to use
something explicit and wise to describe it!)
◦ A method travel_in_time which takes two arguments of type int : start and
arrival and changes the age of the doctor logicaly (Think before coding some
weird arithmetics... Please...). This method also draw a TARDIS on the
standard output. (If you don’t know what a TARDIS is, google it!)
◦ A method use_sonic_screwdriver which prints the following sentence on the
standard output : Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii
◦ A private method regenerate that sets the hp of the doctor to 100 (the maximum)
• You have to simulate all the methods in the main to provide sufficient testing for
the defence *)

class doctor name age (sidekick: People.people) =
  object
    initializer print_endline "A doctor is born"
    val name = name
    val age = age
    val sidekick = sidekick
    val hp = 100
    method to_string =
      "name: " ^ name ^
      ", hp: " ^ string_of_int hp ^
      ", age: " ^ string_of_int age ^
      ", sidekick: " ^ "(" ^ sidekick#to_string ^ ")"
    method talk = print_endline "Hi! I’m the Doctor!"
    method travel_in_time (start: int) (arrival: int) =
      print_endline "           ___
           | |
           | |
  -------------------
  -------------------
   |  ___  |  ___  |
   | | | | | | | | |
   | |-+-| | |-+-| |
   | |_|_| | |_|_| |
   |  ___  |  ___  |
   | |   | | |   | |
   | |   | | |   | |
   | |___| | |___| |
   |  ___  |  ___  |
   | |   | | |   | |
   | |   | | |   | |
   | |___| | |___| |
   |       |       |
  ===================";
      {< age = age + (arrival - start)>}
    method use_sonic_screwdriver = print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"
    method private regenerate = new doctor name age sidekick
  end