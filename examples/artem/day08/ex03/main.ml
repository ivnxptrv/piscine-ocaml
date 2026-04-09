let () =
  (* return *)
  assert (Try.return 42 = Try.Success 42);
  assert (Try.return "hello" = Try.Success "hello");
  print_endline "return: OK";

  (* bind - success *)
  assert (
    Try.bind (Try.Success 10) (fun x -> Try.Success (x * 2)) = Try.Success 20);
  print_endline "bind success: OK";

  (* bind - failure propagation *)
  let exn = Failure "err" in
  assert (
    Try.bind (Try.Failure exn) (fun x -> Try.Success (x + 1)) = Try.Failure exn);
  print_endline "bind failure propagation: OK";

  (* bind - catches exception from f *)
  let result = Try.bind (Try.Success 1) (fun _ -> raise (Failure "boom")) in
  (match result with
  | Try.Failure e -> assert (Printexc.to_string e = "Failure(\"boom\")")
  | Try.Success _ -> assert false);
  print_endline "bind catches exception: OK";

  (* recover - from failure *)
  let recovered =
    Try.recover (Try.Failure (Failure "err")) (fun _ -> Try.Success 99)
  in
  assert (recovered = Try.Success 99);
  print_endline "recover from failure: OK";

  (* recover - success unchanged *)
  assert (Try.recover (Try.Success 42) (fun _ -> Try.Success 0) = Try.Success 42);
  print_endline "recover success unchanged: OK";

  (* filter - predicate true *)
  assert (Try.filter (Try.Success 10) (fun x -> x > 5) = Try.Success 10);
  print_endline "filter predicate true: OK";

  (* filter - predicate false *)
  assert (
    Try.filter (Try.Success 3) (fun x -> x > 5) = Try.Failure Try.Filtered_out);
  print_endline "filter predicate false: OK";

  (* filter - failure propagation *)
  let exn = Failure "original" in
  assert (Try.filter (Try.Failure exn) (fun _ -> true) = Try.Failure exn);
  print_endline "filter failure propagation: OK";

  (* flatten - Success (Success x) *)
  assert (Try.flatten (Try.Success (Try.Success 42)) = Try.Success 42);
  print_endline "flatten Success(Success): OK";

  (* flatten - Success (Failure e) *)
  let exn = Failure "inner" in
  assert (Try.flatten (Try.Success (Try.Failure exn)) = Try.Failure exn);
  print_endline "flatten Success(Failure): OK";

  (* flatten - Failure e *)
  let exn = Failure "outer" in
  assert (Try.flatten (Try.Failure exn) = Try.Failure exn);
  print_endline "flatten Failure: OK";

  print_endline "All Try tests passed!"
