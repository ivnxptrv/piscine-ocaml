let jokes =
  [|
    "What do you call a fish with no eyes? A fsh.";
    "What do you call a deer with no eyes? No eye-deer. ";
    "What do you call a deer with no eyes and no legs? Still no eye-deer.";
    "I told my wife she was drawing her eyebrows too high. She looked \
     surprised.";
    "I used to hate facial hair, but then it grew on me.";
  |]

let () =
  Random.self_init ();
  let joke = jokes.(Random.int (Array.length jokes)) in
  print_endline joke
