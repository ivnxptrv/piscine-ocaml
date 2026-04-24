module type PAIR = sig
  val pair : int * int
end

module type VAL = sig
  val x : int
end

(* FIX ME !!! *)
module Pair : PAIR = struct
  let pair = (21, 42)
end

module type MAKEPROJECTION = functor (P : PAIR) -> VAL

module MakeFst : MAKEPROJECTION =
functor
  (Module : PAIR)
  ->
  struct
    let x, _ = Module.pair
  end

module MakeSnd : MAKEPROJECTION =
functor
  (Module : PAIR)
  ->
  struct
    let _, x = Module.pair
  end

module Fst : VAL = MakeFst (Pair)

module Snd : VAL = MakeSnd (Pair)

let () = Printf.printf "Fst.x = %d, Snd.x = %d\n" Fst.x Snd.x
