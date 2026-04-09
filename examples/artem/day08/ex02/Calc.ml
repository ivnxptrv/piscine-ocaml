module F (M : Monoid.S) = struct
  let add x y = M.add x y
  let sub x y = M.sub x y
  let mul x y = M.mul x y
  let div x y = M.div x y

  let power x n =
    let rec aux acc base n =
      match n with
      | 0 -> acc
      | n when n mod 2 = 1 -> aux (M.mul acc base) (M.mul base base) (n / 2)
      | n -> aux acc (M.mul base base) (n / 2)
    in
    aux M.zero2 x n

  let fact x =
    let rec aux acc n =
      if n = M.zero1 then acc else aux (M.mul acc n) (M.sub n M.zero2)
    in
    aux M.zero2 x
end
