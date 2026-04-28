module Number = struct
  type t = int

  let zero = 0
  let add x y = x + y
end

module Clock = struct
  type t = int

  let zero = 12
  let add x y = (x + y) mod 12
end

module String = struct
  type t = string

  let zero = ""
  let add x y = x ^ y
end

module Func = struct
  type 'a t = 'a -> 'a

  let zero = fun x -> x
  let add f g x = f (g x)
end
