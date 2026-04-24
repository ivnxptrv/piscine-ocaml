module type FIXED = sig
  type t

  val of_float : float -> t

  val of_int : int -> t

  val to_float : t -> float

  val to_int : t -> int

  val to_string : t -> string

  val zero : t

  val one : t

  val succ : t -> t

  val pred : t -> t

  val min : t -> t -> t

  val max : t -> t -> t

  val gth : t -> t -> bool

  val lth : t -> t -> bool

  val gte : t -> t -> bool

  val lte : t -> t -> bool

  (* physical equality *)
  val eqp : t -> t -> bool

  (* structural equality *)
  val eqs : t -> t -> bool

  val add : t -> t -> t

  val sub : t -> t -> t

  val mul : t -> t -> t

  val div : t -> t -> t

  val foreach : t -> t -> (t -> unit) -> unit
end

module type FRACTIONAL_BITS = sig
  val bits : int
end

module type MAKE = functor (Module : FRACTIONAL_BITS) -> FIXED

module Make : MAKE =
functor
  (Module : FRACTIONAL_BITS)
  ->
  struct
    type t = int

    let of_float v =
      let scaled = v *. float_of_int (1 lsl Module.bits) in
      int_of_float (if scaled >= 0.0 then scaled +. 0.5 else scaled -. 0.5)

    let of_int v = v lsl Module.bits

    let to_float v = float_of_int v /. float_of_int (1 lsl Module.bits)

    let to_int v = v asr Module.bits

    let to_string v = string_of_float (to_float v)

    let zero = of_int 0

    let one = of_int 1

    let succ v = v + 1

    let pred v = v - 1

    let min x y = if x < y then x else y

    let max x y = if x > y then x else y

    let gth x y = x > y

    let lth x y = x < y

    let gte x y = x >= y

    let lte x y = x <= y

    let eqp x y = x == y

    let eqs x y = x = y

    let add x y = x + y

    let sub x y = x - y

    let mul x y = (x * y) asr Module.bits

    let div x y = (x lsl Module.bits) / y

    let foreach x y f =
      let rec loop i =
        if i <= y then begin
          f i ;
          loop (succ i)
        end
      in
      loop x
  end

module Fixed4 : FIXED = Make (struct
  let bits = 4
end)

module Fixed8 : FIXED = Make (struct
  let bits = 8
end)

let () =
  print_endline "=== Required output ===" ;
  let x8 = Fixed8.of_float 21.10 in
  let y8 = Fixed8.of_float 21.32 in
  let r8 = Fixed8.add x8 y8 in
  print_endline (Fixed8.to_string r8) ;
  Fixed4.foreach Fixed4.zero Fixed4.one (fun f ->
      print_endline (Fixed4.to_string f) ) ;
  print_endline "\n=== Testing all FIXED functions ===" ;
  (* of_float, to_float, to_string *)
  print_endline "\n-- of_float, to_float, to_string --" ;
  let a = Fixed8.of_float 3.5 in
  Printf.printf "of_float 3.5 -> to_float: %f, to_string: %s\n"
    (Fixed8.to_float a) (Fixed8.to_string a) ;
  (* of_int, to_int *)
  print_endline "\n-- of_int, to_int --" ;
  let b = Fixed8.of_int 5 in
  Printf.printf "of_int 5 -> to_int: %d, to_float: %f\n" (Fixed8.to_int b)
    (Fixed8.to_float b) ;
  Printf.printf "to_int 3.5 -> %d\n" (Fixed8.to_int (Fixed8.of_float 3.5)) ;
  Printf.printf "to_int 3.25 -> %d\n" (Fixed8.to_int (Fixed8.of_float 3.25)) ;
  (* zero, one *)
  print_endline "\n-- zero, one --" ;
  Printf.printf "zero: %s, one: %s\n"
    (Fixed8.to_string Fixed8.zero)
    (Fixed8.to_string Fixed8.one) ;
  (* succ, pred *)
  print_endline "\n-- succ, pred --" ;
  Printf.printf "succ(zero): %s\n" (Fixed8.to_string (Fixed8.succ Fixed8.zero)) ;
  Printf.printf "pred(one): %s\n" (Fixed8.to_string (Fixed8.pred Fixed8.one)) ;
  (* min, max *)
  print_endline "\n-- min, max --" ;
  let x = Fixed8.of_float 2.5 in
  let y = Fixed8.of_float 7.25 in
  Printf.printf "min(2.5, 7.25): %s\n" (Fixed8.to_string (Fixed8.min x y)) ;
  Printf.printf "max(2.5, 7.25): %s\n" (Fixed8.to_string (Fixed8.max x y)) ;
  (* gth, lth, gte, lte *)
  print_endline "\n-- gth, lth, gte, lte --" ;
  Printf.printf "2.5 > 7.25: %b\n" (Fixed8.gth x y) ;
  Printf.printf "2.5 < 7.25: %b\n" (Fixed8.lth x y) ;
  Printf.printf "2.5 >= 2.5: %b\n" (Fixed8.gte x x) ;
  Printf.printf "2.5 <= 7.25: %b\n" (Fixed8.lte x y) ;
  (* eqp, eqs *)
  print_endline "\n-- eqp (physical), eqs (structural) --" ;
  let z = Fixed8.of_float 2.5 in
  Printf.printf "x eqs z (both 2.5): %b\n" (Fixed8.eqs x z) ;
  Printf.printf "x eqp x (same value): %b\n" (Fixed8.eqp x x) ;
  (* add, sub *)
  print_endline "\n-- add, sub --" ;
  Printf.printf "2.5 + 7.25 = %s\n" (Fixed8.to_string (Fixed8.add x y)) ;
  Printf.printf "7.25 - 2.5 = %s\n" (Fixed8.to_string (Fixed8.sub y x)) ;
  (* mul, div *)
  print_endline "\n-- mul, div --" ;
  let m = Fixed8.of_float 3.0 in
  let n = Fixed8.of_float 4.0 in
  Printf.printf "3.0 * 4.0 = %s\n" (Fixed8.to_string (Fixed8.mul m n)) ;
  Printf.printf "12.0 / 4.0 = %s\n"
    (Fixed8.to_string (Fixed8.div (Fixed8.of_float 12.0) n)) ;
  (* foreach *)
  print_endline "\n-- foreach (0 to 0.5 with Fixed4) --" ;
  let half = Fixed4.of_float 0.5 in
  Fixed4.foreach Fixed4.zero half (fun f ->
      Printf.printf "%s " (Fixed4.to_string f) ) ;
  print_endline "" ;
  (* negative numbers *)
  print_endline "\n=== Testing negative numbers ===" ;
  print_endline "\n-- of_float, to_float with negatives --" ;
  let neg1 = Fixed8.of_float (-3.5) in
  let neg2 = Fixed8.of_float (-2.25) in
  Printf.printf "of_float -3.5 -> to_float: %f, to_string: %s\n"
    (Fixed8.to_float neg1) (Fixed8.to_string neg1) ;
  Printf.printf "of_float -2.25 -> to_float: %f, to_string: %s\n"
    (Fixed8.to_float neg2) (Fixed8.to_string neg2) ;
  print_endline "\n-- of_int, to_int with negatives --" ;
  let neg_int = Fixed8.of_int (-5) in
  Printf.printf "of_int(-5) -> to_float: %f, to_int: %d\n"
    (Fixed8.to_float neg_int) (Fixed8.to_int neg_int) ;
  Printf.printf "to_int(-3.5): %d\n" (Fixed8.to_int neg1) ;
  Printf.printf "to_int(-2.25): %d\n" (Fixed8.to_int neg2) ;
  print_endline "\n-- add, sub with negatives --" ;
  Printf.printf "-3.5 + (-2.25) = %s\n"
    (Fixed8.to_string (Fixed8.add neg1 neg2)) ;
  Printf.printf "-3.5 - (-2.25) = %s\n"
    (Fixed8.to_string (Fixed8.sub neg1 neg2)) ;
  Printf.printf "2.5 + (-3.5) = %s\n" (Fixed8.to_string (Fixed8.add x neg1)) ;
  print_endline "\n-- mul, div with negatives --" ;
  Printf.printf "-3.5 * 2.0 = %s\n"
    (Fixed8.to_string (Fixed8.mul neg1 (Fixed8.of_float 2.0))) ;
  Printf.printf "-3.5 / 2.0 = %s\n"
    (Fixed8.to_string (Fixed8.div neg1 (Fixed8.of_float 2.0))) ;
  print_endline "\n-- comparisons with negatives --" ;
  Printf.printf "-3.5 < -2.25: %b\n" (Fixed8.lth neg1 neg2) ;
  Printf.printf "-3.5 > -2.25: %b\n" (Fixed8.gth neg1 neg2) ;
  Printf.printf "-3.5 < 2.5: %b\n" (Fixed8.lth neg1 x) ;
  print_endline "\n-- min, max with negatives --" ;
  Printf.printf "min(-3.5, -2.25): %s\n"
    (Fixed8.to_string (Fixed8.min neg1 neg2)) ;
  Printf.printf "max(-3.5, 2.5): %s\n" (Fixed8.to_string (Fixed8.max neg1 x)) ;
  print_endline "\n-- division by zero --" ;
  try
    let _ = Fixed8.div (Fixed8.of_float 1.0) Fixed8.zero in
    print_endline "1.0 / 0: no exception (unexpected)"
  with Division_by_zero ->
    print_endline "1.0 / 0: Division_by_zero exception (expected)"
