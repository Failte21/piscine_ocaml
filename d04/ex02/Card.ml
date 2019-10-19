module Color =
struct
  type t = Spade | Heart | Diamond | Club

  let all = [Spade; Heart; Diamond; Club]

  let toString = function
    | Spade -> "S"
    | Heart -> "H"
    | Diamond -> "D"
    | Club -> "C"

  let toStringVerbose = function
  | Spade -> "Spade"
  | Heart -> "Heart"
  | Diamond -> "Diamond"
  | Club -> "Club"
end

module Value =
struct
  type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

  let all = [T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As]

  let toInt = function
    | T2 -> 2
    | T3 -> 3 
    | T4 -> 4 
    | T5 -> 5 
    | T6 -> 6 
    | T7 -> 7 
    | T8 -> 8 
    | T9 -> 9 
    | T10 -> 10 
    | Jack -> 11 
    | Queen -> 12
    | King -> 13 
    | As -> 14

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
end

type t = {
  value: Value.t;
  color: Color.t
}

let newCard value color = {
  value = value;
  color = color
}

let toString card =
  (Value.toString card.value) ^ (Color.toString card.color)

let toStringVerbose card =
  "Card(" ^
  (Value.toStringVerbose card.value) ^
  ", " ^
  (Color.toStringVerbose card.color) ^
  ")"

let compare card_a card_b =
  if card_a.value < card_b.value then -1 else 
  if card_a.value > card_b.value then 1 else 
  0

let max card_a card_b =
  let cmp_res = compare card_a card_b in
  if cmp_res = -1 then card_b else
  if cmp_res = 1 then card_a else
  card_a

let min card_a card_b =
  let cmp_res = compare card_a card_b in
  if cmp_res = -1 then card_a else
  if cmp_res = 1 then card_b else
  card_a

let best = function
  | [] -> invalid_arg "List is empty"
  | cards -> List.fold_left max (List.hd cards) cards

let isOf card color = card.color = color 
let isSpade card = isOf card Spade
let isHeart card = isOf card Heart
let isDiamond card = isOf card Diamond
let isClub card = isOf card Club

let getValue card = card.value 
let getColor card = card.color

let all =
  List.fold_left (fun acc e -> acc @ e) []
  (List.map (fun c -> List.map (fun v -> (newCard v c)) Value.all) Color.all)

let allSpades = List.filter isSpade all
let allHearts = List.filter isHeart all 
let allDiamonds = List.filter isDiamond all
let allClubs = List.filter isClub all