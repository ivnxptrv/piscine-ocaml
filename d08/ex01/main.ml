let print_proj (t, s, g) = Printf.printf "(\"%s\", \"%s\", %d)" t s g

let test_project name (t, s, g) (et, es, eg) =
  let pass = t = et && s = es && g = eg in
  Printf.printf "%s = " name ;
  print_proj (t, s, g) ;
  Printf.printf " (expected " ;
  print_proj (et, es, eg) ;
  Printf.printf "): %s\n" (if pass then "OK" else "FAIL")

let () =
  let p1 : App.project = ("des", "succeed", 80) in
  print_proj p1 ;
  let p1 : App.project = ("des", "succeed", 80) in
  let p2 : App.project = App.fail p1 in
  let res = App.combine p1 p2 in
  print_proj res ;
  print_newline () ;
  (* Test zero *)
  Printf.printf "=== zero ===\n" ;
  test_project "zero" App.zero ("", "", 0) ;
  (* Test combine *)
  Printf.printf "\n=== combine ===\n" ;
  test_project "combine zero zero"
    (App.combine App.zero App.zero)
    ("", "fail", 0) ;
  test_project "combine (\"web\", \"succeed\", 90) (\"app\", \"succeed\", 80)"
    (App.combine ("web", "succeed", 90) ("app", "succeed", 80))
    ("webapp", "succeed", 85) ;
  test_project "combine (\"web\", \"succeed\", 90) (\"app\", \"fail\", 60)"
    (App.combine ("web", "succeed", 90) ("app", "fail", 60))
    ("webapp", "fail", 75) ;
  test_project "combine (\"a\", \"succeed\", 80) (\"b\", \"succeed\", 80)"
    (App.combine ("a", "succeed", 80) ("b", "succeed", 80))
    ("ab", "succeed", 80) ;
  test_project "combine (\"a\", \"fail\", 79) (\"b\", \"fail\", 79)"
    (App.combine ("a", "fail", 79) ("b", "fail", 79))
    ("ab", "fail", 79) ;
  test_project "combine (\"x\", \"succeed\", 100) (\"y\", \"succeed\", 100)"
    (App.combine ("x", "succeed", 100) ("y", "succeed", 100))
    ("xy", "succeed", 100) ;
  (* Test fail *)
  Printf.printf "\n=== fail ===\n" ;
  test_project "fail zero" (App.fail App.zero) ("", "fail", 0) ;
  test_project "fail (\"web\", \"succeed\", 90)"
    (App.fail ("web", "succeed", 90))
    ("web", "fail", 0) ;
  test_project "fail (\"app\", \"fail\", 50)"
    (App.fail ("app", "fail", 50))
    ("app", "fail", 0) ;
  (* Test succeed *)
  Printf.printf "\n=== succeed ===\n" ;
  test_project "succeed zero" (App.success App.zero) ("", "succeed", 80) ;
  test_project "succeed (\"web\", \"fail\", 0)"
    (App.success ("web", "fail", 0))
    ("web", "succeed", 80) ;
  test_project "succeed (\"app\", \"succeed\", 100)"
    (App.success ("app", "succeed", 100))
    ("app", "succeed", 80)
