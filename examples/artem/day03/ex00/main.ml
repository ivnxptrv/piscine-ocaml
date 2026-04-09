let print_color color =
  print_endline (Color.toString color ^ " - " ^ Color.toStringVerbose color)

let () =
  let colors = Color.all in
  List.iter print_color colors
