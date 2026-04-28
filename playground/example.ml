module Option = struct
  type 'a t = Some of 'a | None

  let return x = Some x  (* тривиальная упаковка *)
  let bind m f =
    match m with
    | Some x -> f x  (* извлекаем значение и применяем функцию *)
    | None -> None  (* если нет значения, возвращаем None *)
end

module Lst = struct
  type 'a t = 'a list

  let return x = [x]  (* тривиальная упаковка *)
  let bind m f =
    List.concat (List.map f m)  (* применяем функцию к каждому элементу и объединяем результаты *)
end

(* Инфиксный bind: `m >>= f` — это просто `Option.bind m f` *)
let example_option_bind () =
  let ( >>= ) = Option.bind in
  Option.return 5                                          >>= fun s1 ->
  None                                                     >>= fun s2 ->
  (if s2 > 5 then Option.return (s2 * 2) else Option.None) >>= fun s3 ->
  Option.return s3

(* То же самое через let* — синтаксический сахар над Option.bind *)
let example_option_let () =
  let ( let* ) = Option.bind in
  let* s1 = Option.return 5 in
  let* s2 = None in
  let* s3 = if s2 > 5 then Option.return (s2 * 2) else Option.None in
  Option.return s3

(* Список как монада: bind — это flat_map. Тоже через инфикс *)
let example_list_bind () =
  let ( >>= ) = Lst.bind in
  [1; 2; 3] >>= fun x ->
  [10; 20]  >>= fun y ->
  Lst.return (x + y)

(* То же самое через let*, но локально переопределённый под Lst.bind *)
let example_list_let () =
  let ( let* ) = Lst.bind in
  let* x = [1; 2; 3] in
  let* y = [10; 20] in
  Lst.return (x + y)

let () =
  (match example_option_bind () with
   | Option.Some n -> Printf.printf "Option bind: %d\n" n
   | Option.None   -> Printf.printf "Option bind: None\n");
  (match example_option_let () with
   | Option.Some n -> Printf.printf "Option let*: %d\n" n
   | Option.None   -> Printf.printf "Option let*: None\n");
  let show xs =
    "[" ^ String.concat "; " (List.map string_of_int xs) ^ "]"
  in
  Printf.printf "List bind: %s\n" (show (example_list_bind ()));
  Printf.printf "List let*: %s\n" (show (example_list_let ()))
