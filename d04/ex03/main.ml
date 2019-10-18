let green = "\x1b[32m"
let reset = "\x1b[0m"

let deckA = Deck.newDeck ()
let deckB = Deck.newDeck ()
let deckC = Deck.newDeck ()

let rec draw_forever = function
  | deck ->
    let (card, remaining_deck) = Deck.drawCard deck in
    print_endline (Deck.Card.toStringVerbose card);
    draw_forever remaining_deck

let main () =
  Printf.printf "%s-----DECK A TO STRING-----%s\n" green reset;
  print_endline (List.fold_left (fun acc e -> acc ^ e ^ ", ") "" (Deck.toStringList deckA));
  print_char '\n';
  Printf.printf "%s-----DECK B TO STRING VERBOSE-----%s\n" green reset;
  print_endline (List.fold_left (fun acc e -> acc ^ e ^ ", ") "" (Deck.toStringListVerbose deckB));
  print_char '\n';
  Printf.printf "%s-----DRAW A CARD FROM DECK A-----%s\n" green reset;
  let (card, remaining_deck) = Deck.drawCard deckA in
  Printf.printf "Drawn card: %s\n" (Deck.Card.toStringVerbose card);
  Printf.printf "Remaining deck: %s\n" (List.fold_left (fun acc e -> acc ^ e ^ ", ") "" (Deck.toStringList remaining_deck));
  print_char '\n';
  Printf.printf "%s-----EMPTY ALL OF DECK C-----%s\n" green reset;
  draw_forever deckC;
  print_char '\n'

let () = main ()