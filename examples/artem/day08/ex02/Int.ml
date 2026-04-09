module M : Monoid.S with type element = int = struct
  type element = int

  let zero1 = 0
  let zero2 = 1
  let mul x y = x * y
  let add x y = x + y
  let div x y = x / y
  let sub x y = x - y
end
