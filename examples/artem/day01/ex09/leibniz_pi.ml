let leibniz_pi min_d =
  let rec leibniz_pi_aux i acc ref_pi =
    let current_pi = acc *. 4.0 in
    let delta = ref_pi -. current_pi in
    let abs_delta = if delta < 0.0 then -.delta else delta in
    print_float current_pi;
    print_newline ();
    if abs_delta <= min_d then i
    else
      leibniz_pi_aux (i + 1)
        (acc +. ((-1.0 ** float_of_int i) /. ((2.0 *. float_of_int i) +. 1.0)))
        ref_pi
  in
  if min_d < 0.0 then -1 else leibniz_pi_aux 0 0.0 (4.0 *. atan 1.0)

(* Tests *)
let () = print_endline (string_of_int (leibniz_pi 0.0000001))
