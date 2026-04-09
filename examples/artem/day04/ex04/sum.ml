let sum a b = a +. b

let () =
  (* Basic addition *)
  let result1 = sum 2.5 3.7 in
  Printf.printf "sum 2.5 3.7 = %.2f\n" result1;

  let result2 = sum 10.0 5.0 in
  Printf.printf "sum 10.0 5.0 = %.2f\n" result2;

  (* Adding zero *)
  let result3 = sum 42.0 0.0 in
  Printf.printf "sum 42.0 0.0 = %.2f\n" result3;

  (* Negative numbers *)
  let result4 = sum (-5.5) 3.5 in
  Printf.printf "sum (-5.5) 3.5 = %.2f\n" result4;

  let result5 = sum (-10.0) (-20.0) in
  Printf.printf "sum (-10.0) (-20.0) = %.2f\n" result5;

  (* Large numbers *)
  let result6 = sum 1234.56 7890.12 in
  Printf.printf "sum 1234.56 7890.12 = %.2f\n" result6;

  (* Small decimal numbers *)
  let result7 = sum 0.1 0.2 in
  Printf.printf "sum 0.1 0.2 = %.17f\n" result7
