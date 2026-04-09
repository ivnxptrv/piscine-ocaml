module StringCompare = struct
  type t = string
  let compare a b =
    if a < b then -1 else if a > b then 1 else 0
end

module StringSet = Set.Make(StringCompare)

let () =
  let set = List.fold_right StringSet.add [ "foo"; "bar"; "baz"; "qux" ] StringSet.empty in
  StringSet.iter print_endline set;
  print_endline (StringSet.fold ( ^ ) set "")
