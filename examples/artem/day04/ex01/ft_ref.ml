type 't ft_ref = { mutable contents : 't }

let return x = { contents = x }
let get { contents } = contents
let set ref x = ref.contents <- x
let bind ref (f : 'a -> 'b ft_ref) = f (get ref)

let () =
  let ref = return 0 in
  set ref 1;
  print_endline (string_of_int (get ref));
  let ref = bind ref (fun x -> return (x + 1)) in
  print_endline (string_of_int (get ref))
