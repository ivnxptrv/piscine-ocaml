module CalcInt = Calc.F (Int.M)
module CalcFloat = Calc.F (Float.M)

let () =
  print_endline "=== Int Monoid Tests ===";
  Printf.printf "zero1 = %d\n" Int.M.zero1;
  Printf.printf "zero2 = %d\n" Int.M.zero2;
  Printf.printf "add 2 3 = %d\n" (Int.M.add 2 3);
  Printf.printf "sub 5 3 = %d\n" (Int.M.sub 5 3);
  Printf.printf "mul 4 3 = %d\n" (Int.M.mul 4 3);
  Printf.printf "div 10 2 = %d\n" (Int.M.div 10 2);

  print_endline "\n=== Float Monoid Tests ===";
  Printf.printf "zero1 = %f\n" Float.M.zero1;
  Printf.printf "zero2 = %f\n" Float.M.zero2;
  Printf.printf "add 2.5 3.5 = %f\n" (Float.M.add 2.5 3.5);
  Printf.printf "sub 5.0 3.0 = %f\n" (Float.M.sub 5.0 3.0);
  Printf.printf "mul 4.0 3.0 = %f\n" (Float.M.mul 4.0 3.0);
  Printf.printf "div 10.0 2.0 = %f\n" (Float.M.div 10.0 2.0);

  print_endline "\n=== Calc Int Tests ===";
  Printf.printf "add 2 3 = %d\n" (CalcInt.add 2 3);
  Printf.printf "sub 5 3 = %d\n" (CalcInt.sub 5 3);
  Printf.printf "mul 4 3 = %d\n" (CalcInt.mul 4 3);
  Printf.printf "div 10 2 = %d\n" (CalcInt.div 10 2);
  Printf.printf "power 2 10 = %d\n" (CalcInt.power 2 10);
  Printf.printf "power 3 0 = %d\n" (CalcInt.power 3 0);
  Printf.printf "power 5 3 = %d\n" (CalcInt.power 5 3);
  Printf.printf "fact 5 = %d\n" (CalcInt.fact 5);
  Printf.printf "fact 0 = %d\n" (CalcInt.fact 0);
  Printf.printf "fact 10 = %d\n" (CalcInt.fact 10);

  print_endline "\n=== Calc Float Tests ===";
  Printf.printf "add 2.5 3.5 = %f\n" (CalcFloat.add 2.5 3.5);
  Printf.printf "sub 5.0 3.0 = %f\n" (CalcFloat.sub 5.0 3.0);
  Printf.printf "mul 4.0 3.0 = %f\n" (CalcFloat.mul 4.0 3.0);
  Printf.printf "div 10.0 2.0 = %f\n" (CalcFloat.div 10.0 2.0);
  Printf.printf "power 2.0 10 = %f\n" (CalcFloat.power 2.0 10);
  Printf.printf "power 3.0 0 = %f\n" (CalcFloat.power 3.0 0);
  Printf.printf "power 0.5 3 = %f\n" (CalcFloat.power 0.5 3);
  Printf.printf "fact 5.0 = %f\n" (CalcFloat.fact 5.0);
  Printf.printf "fact 0.0 = %f\n" (CalcFloat.fact 0.0);
  Printf.printf "fact 10.0 = %f\n" (CalcFloat.fact 10.0)
