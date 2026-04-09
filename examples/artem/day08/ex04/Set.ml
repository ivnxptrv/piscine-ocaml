type 'a t = 'a list

let return x = [ x ]

let bind s f =
  let result = List.concat_map f s in
  List.sort_uniq compare result

let union s1 s2 =
  let result = List.concat [ s1; s2 ] in
  List.sort_uniq compare result

let inter s1 s2 = List.filter (fun el -> List.mem el s2) s1
let diff s1 s2 = List.filter (fun el -> not (List.mem el s2)) s1
let filter s f = List.filter f s
let foreach s f = List.iter f s
let for_all s f = List.for_all f s
let exists s f = List.exists f s
