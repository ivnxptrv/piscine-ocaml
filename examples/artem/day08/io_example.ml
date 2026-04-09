module IO = struct
  type 'a t = Io of (unit -> 'a)

  let return x = Io (fun () -> x)
  let bind (Io thunk) (f : 'a -> 'b t) = Io (fun () ->
    let x = thunk () in
    let (Io g) = f x in
    g ()
  )
  let run (Io thunk) = thunk ()
end

let ( >>= ) = IO.bind

let io_print_endline s = IO.Io (fun () -> print_endline s)
let io_read_line = IO.Io (fun () -> read_line ())

let () =
  IO.run (
    io_print_endline "Enter your name: "      >>= fun () ->
    io_read_line                              >>= fun name ->
    io_print_endline ("Hello, " ^ name ^ "!")
  )
