let print_card card =
  Printf.printf "%s - %s\n" (Card.toString card) (Card.toStringVerbose card)

(* let () = *)
(*   let card1 = Card.newCard Heart As in *)
(*   let card2 = Card.newCard Spade King in *)
(*   let card3 = Card.newCard Diamond Queen in *)
(*   let card4 = Card.newCard Club Jack in *)

(*   let cards1 = [ card1; card2; card3; card4 ] in *)
(*   let cards2 = Card.all in *)

(*   let min_card = Card.min card1 card2 in *)
(*   let max_card = Card.max card3 card4 in *)
(*   let best_card = Card.best cards1 in *)

(*   print_card min_card; *)
(*   print_card max_card; *)
(*   print_card best_card; *)
(*   print_newline (); *)

(*   List.iter print_card cards2 *)

let () =
  let card1 = Card.newCard As Heart in
  let card2 = Card.newCard King Spade in
  let card3 = Card.newCard Queen Diamond in
  let card4 = Card.newCard Jack Club in
  let cards1 = [card1; card2; card3; card4] in
  let cards2 = Card.all in
  let min_card = Card.min card1 card2 in
  let max_card = Card.max card3 card4 in
  let best_card = Card.best cards1 in
  print_card min_card ;
  print_card max_card ;
  print_card best_card ;
  print_newline () ;
  List.iter print_card cards2
