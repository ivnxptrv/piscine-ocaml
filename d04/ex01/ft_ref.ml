type 'a ft_ref = {mutable contents: 'a}

(* let my_val : int ft_ref = {contents= 5} *)

let return x = {contents= x}

let get my_ref = my_ref.contents

let set my_ref contents = my_ref.contents <- contents

let bind my_ref f = f my_ref.contents

let () =
  let my_ref = return 1 in
  print_endline (string_of_int (get my_ref)) ;
  let my_ref = return 11 in
  set my_ref 42 ;
  print_endline (string_of_int (get my_ref)) ;
  let my_ref = return 111 in
  (* we take value of my_ref mutate it and create anotehr my_ref2 container and put it there *)
  let my_ref2 = bind my_ref (fun x -> return (x + 1)) in
  print_endline (string_of_int (get my_ref2))
