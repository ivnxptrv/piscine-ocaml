type 'a monad = Monad of 'a

let return x = Monad x

let bind (Monad x) (f : 'a -> 'b monad) = f x

let ( >>= ) = bind

let unwrap (Monad x) = x

let () =
  let result =
    return 42      >>= fun x ->
    return (x + 2) >>= fun y ->
    return (x * y)
  in
  Printf.printf "Result: %d\n" (unwrap result)
