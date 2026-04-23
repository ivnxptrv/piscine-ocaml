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

let all : t list = [T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As]

let toInt = function
  | T2 ->
      1
  | T3 ->
      2
  | T4 ->
      3
  | T5 ->
      4
  | T6 ->
      5
  | T7 ->
      6
  | T8 ->
      7
  | T9 ->
      8
  | T10 ->
      9
  | Jack ->
      10
  | Queen ->
      11
  | King ->
      12
  | As ->
      13

let toString = function
  | T2 ->
      "2"
  | T3 ->
      "3"
  | T4 ->
      "4"
  | T5 ->
      "5"
  | T6 ->
      "6"
  | T7 ->
      "7"
  | T8 ->
      "8"
  | T9 ->
      "9"
  | T10 ->
      "10"
  | Jack ->
      "J"
  | Queen ->
      "Q"
  | King ->
      "K"
  | As ->
      "A"

let toStringVerbose = function
  | Jack ->
      "Jack"
  | Queen ->
      "Queen"
  | King ->
      "King"
  | As ->
      "As"
  | x ->
      toString x

let next = function
  | T2 ->
      T3
  | T3 ->
      T4
  | T4 ->
      T5
  | T5 ->
      T6
  | T6 ->
      T7
  | T7 ->
      T8
  | T8 ->
      T9
  | T9 ->
      T10
  | T10 ->
      Jack
  | Jack ->
      Queen
  | Queen ->
      King
  | King ->
      As
  | As ->
      invalid_arg "As has no next value"

let previous = function
  | T2 ->
      invalid_arg "T2 has no previous value"
  | T3 ->
      T2
  | T4 ->
      T3
  | T5 ->
      T4
  | T6 ->
      T5
  | T7 ->
      T6
  | T8 ->
      T7
  | T9 ->
      T8
  | T10 ->
      T9
  | Jack ->
      T10
  | Queen ->
      Jack
  | King ->
      Queen
  | As ->
      King

(* type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As *)
(* (\** The list of all values of type t *\) *)
(* val all : t list *)
(* (\** Interger representation of a card value, from 1 for T2 to 13 for As *\) *)
(* val toInt : t -> int *)
(* (\** returns "2", ..., "10", "J", "Q", "K" or "A" *\) *)
(* val toString : t -> string *)
(* (\** returns "2", ..., "10", "Jack", "Queen", "King" or "As" *\) *)
(* val toStringVerbose : t -> string *)
(* (\** Returns the next value, or calls invalid_arg if argument is As *\) *)
(* val next : t -> t *)
(* (\** Returns the previous value, or calls invalid_arg if argument is T2 *\) *)
(* val previous : t -> t *)
