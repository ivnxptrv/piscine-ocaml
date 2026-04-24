(* type point = float array *)

(* takes two points and calculates the Euclidian distance between them *)
(* Euclidean distance is the generalization of the "straight-line" distance we use in our 3D world *)
(* applied to any number of dimensions n *)
let eu_dist (p1 : float array) (p2 : float array) : float =
  let sum = ref 0.0 in
  Array.iter2
    (fun p1_coordinate p2_coordinate ->
      sum := !sum +. ((p1_coordinate -. p2_coordinate) ** 2.0) )
    p1 p2 ;
  sqrt !sum

let () =
  (* Basic 2D distance *)
  let result1 = eu_dist [|1.0; 2.0|] [|3.0; 4.0|] in
  Printf.printf "eu_dist [|1.0; 2.0|] [|3.0; 4.0|] = %.2f\n" result1 ;
  (* Negative numbers *)
  let result2 = eu_dist [|1.0; -2.0|] [|3.0; -4.0|] in
  Printf.printf "eu_dist [|1.0; -2.0|] [|3.0; -4.0|] = %.2f\n" result2 ;
  (* Zero vector *)
  let result3 = eu_dist [|0.0; 0.0|] [|0.0; 0.0|] in
  Printf.printf "eu_dist [|0.0; 0.0|] [|0.0; 0.0|] = %.2f\n" result3 ;
  (* Large numbers *)
  let result4 = eu_dist [|1000000.0; 2000000.0|] [|3000000.0; 4000000.0|] in
  Printf.printf
    "eu_dist [|1000000.0; 2000000.0|] [|3000000.0; 4000000.0|] = %.2f\n" result4 ;
  (* Small decimal numbers *)
  let result5 = eu_dist [|0.1; 0.2|] [|0.3; 0.4|] in
  Printf.printf "eu_dist [|0.1; 0.2|] [|0.3; 0.4|] = %.17f\n" result5 ;
  (* 3D vectors *)
  let result6 = eu_dist [|1.0; 2.0; 3.0|] [|4.0; 5.0; 6.0|] in
  Printf.printf "eu_dist [|1.0; 2.0; 3.0|] [|4.0; 5.0; 6.0|] = %.2f\n" result6 ;
  (* Same point - distance should be 0 *)
  let result7 = eu_dist [|5.5; 7.3|] [|5.5; 7.3|] in
  Printf.printf "eu_dist [|5.5; 7.3|] [|5.5; 7.3|] = %.2f\n" result7
