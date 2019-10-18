type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

let all = [T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As]

let toString = function
  | T2 -> "2"
  | T3 -> "3" 
  | T4 -> "4" 
  | T5 -> "5" 
  | T6 -> "6" 
  | T7 -> "7" 
  | T8 -> "8" 
  | T9 -> "9" 
  | T10 -> "10" 
  | Jack -> "J" 
  | Queen -> "Q" 
  | King -> "K" 
  | As -> "A"

let toStringVerbose = function
  | T2 -> "2"
  | T3 -> "3" 
  | T4 -> "4" 
  | T5 -> "5" 
  | T6 -> "6" 
  | T7 -> "7" 
  | T8 -> "8" 
  | T9 -> "9" 
  | T10 -> "10" 
  | Jack -> "Jack" 
  | Queen -> "Queen" 
  | King -> "King" 
  | As -> "As"

let next = function
  | T2 -> T3
  | T3 -> T4
  | T4 -> T5
  | T5 -> T6
  | T6 -> T7
  | T7 -> T8
  | T8 -> T9
  | T9 -> T10
  | T10 -> Jack
  | Jack -> Queen
  | Queen -> King
  | King -> As
  | As -> invalid_arg "As is the last card of the deck"


let previous = function
| T2 -> invalid_arg "2 is the first card of the deck"
| T3 -> T2
| T4 -> T3
| T5 -> T4
| T6 -> T5
| T7 -> T6
| T8 -> T7
| T9 -> T8
| T10 -> T9
| Jack -> T10
| Queen -> Jack
| King -> Queen
| As -> King

let without_as = List.filter (fun e -> e != As) all
let without_2 = List.filter (fun e -> e != T2) all