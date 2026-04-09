exception Filtered_out

type 'a t = Success of 'a | Failure of exn

let return x = Success x

let bind x f =
  match x with
  | Failure e -> Failure e
  | Success x -> ( try f x with e -> Failure e)

let recover x f = match x with Failure e -> f e | Success x -> Success x

let filter x pred =
  match x with
  | Failure e -> Failure e
  | Success x when pred x -> Success x
  | Success _ -> Failure Filtered_out

let flatten x =
  match x with
  | Failure e -> Failure e
  | Success (Success x) -> Success x
  | Success (Failure e) -> Failure e
