(* Allowed functions : Pervasives and List modules *)

(* Write a parameterized class army that has the following attributes :
◦ A member attribute of type ’a list which contains a list of instance of one of
the 3 previous classes.
◦ An add method that adds an instance ot the list (front or back).
◦ A delete method that removes the head of the list member. (front or back)
• You have to simulate a construction and a destruction of an army of each type in
the main to provide sufficient testing for the defence. *)

class ['a] army (member: 'a list) =
  object
    val member = member
    method get_member = member 
    method add (soldier: 'a) = {< member = member @ [soldier]>}
    method delete = {< member = List.tl member>}
  end