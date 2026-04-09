let drawCards deck n =
  let rec drawCards' deck n =
    if n = 0 then ()
    else
      let hd, tl = Deck.drawCard deck in
      print_endline (Deck.Card.toStringVerbose hd);
      drawCards' tl (n - 1)
  in
  drawCards' deck n

let () =
  let deck = Deck.newDeck () in
  List.iter print_endline (Deck.toStringList deck);
  print_newline ();
  List.iter print_endline (Deck.toStringListVerbose deck);
  print_newline ();
  drawCards deck 10
