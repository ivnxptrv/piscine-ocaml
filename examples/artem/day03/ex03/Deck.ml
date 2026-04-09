module Card = struct
  module Color = struct
    type t = Spade | Heart | Diamond | Club

    let all = [ Spade; Heart; Diamond; Club ]

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

  module Value = struct
    type t =
      | T2
      | T3
      | T4
      | T5
      | T6
      | T7
      | T8
      | T9
      | T10
      | Jack
      | Queen
      | King
      | As

    let all = [ T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As ]

    let toInt = function
      | T2 -> 1
      | T3 -> 2
      | T4 -> 3
      | T5 -> 4
      | T6 -> 5
      | T7 -> 6
      | T8 -> 7
      | T9 -> 8
      | T10 -> 9
      | Jack -> 10
      | Queen -> 11
      | King -> 12
      | As -> 13

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

    let toStringVerbose v =
      match v with
      | Jack -> "Jack"
      | Queen -> "Queen"
      | King -> "King"
      | As -> "As"
      | _ -> toString v

    let next v =
      match v with
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
      | As -> invalid_arg "As has no next value"

    let previous v =
      match v with
      | T2 -> invalid_arg "T2 has no previous value"
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

  type t = { value : Value.t; color : Color.t }

  let newCard value color = { value; color }
  let allSpades = List.map (fun v -> newCard v Spade) Value.all
  let allHearts = List.map (fun v -> newCard v Heart) Value.all
  let allDiamonds = List.map (fun v -> newCard v Diamond) Value.all
  let allClubs = List.map (fun v -> newCard v Club) Value.all
  let all = List.concat [ allSpades; allHearts; allDiamonds; allClubs ]
  let getValue card = card.value
  let getColor card = card.color

  let toString card =
    Printf.sprintf "%s%s"
      (Value.toString card.value)
      (Color.toString card.color)

  let toStringVerbose card =
    Printf.sprintf "Card(%s, %s)"
      (Value.toStringVerbose card.value)
      (Color.toStringVerbose card.color)

  let compare card1 card2 =
    let value1 = Value.toInt card1.value in
    let value2 = Value.toInt card2.value in
    value1 - value2

  let min card1 card2 = if compare card1 card2 <= 0 then card1 else card2
  let max card1 card2 = if compare card1 card2 >= 0 then card1 else card2

  let best cards =
    match cards with
    | [] -> invalid_arg "best called with empty list"
    | hd :: tl ->
        List.fold_left
          (fun acc card -> if compare card acc > 0 then card else acc)
          hd tl

  let isOf card color = card.color = color
  let isSpade card = isOf card Spade
  let isHeart card = isOf card Heart
  let isDiamond card = isOf card Diamond
  let isClub card = isOf card Club
end

type t = Card.t list

let newDeck () =
  let rnd = Random.State.make_self_init () in
  let cards_with_random =
    List.map (fun card -> (Random.State.bits rnd, card)) Card.all
  in
  let sorted = List.sort compare cards_with_random in
  List.map snd sorted

let toStringList cards = List.map Card.toString cards
let toStringListVerbose cards = List.map Card.toStringVerbose cards

let drawCard deck =
  match deck with [] -> raise (Failure "Deck is empty") | hd :: tl -> (hd, tl)
