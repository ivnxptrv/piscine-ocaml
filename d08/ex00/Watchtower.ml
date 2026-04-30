module type Watchtower = sig
  type hour = int

  val zero : hour

  val add : hour -> hour -> hour

  val sub : hour -> hour -> hour
end

type hour = int

let zero : hour = 12

let add h1 h2 = (h1 + h2) mod 12

let sub h1 h2 = (((h1 - h2) mod 12) + 12) mod 12
