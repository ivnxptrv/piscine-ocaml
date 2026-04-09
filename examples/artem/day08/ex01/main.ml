let print_proj (t, s, g) = Printf.printf "(\"%s\", \"%s\", %d)" t s g

let test_project name (t, s, g) (et, es, eg) =
  let pass = t = et && s = es && g = eg in
  Printf.printf "%s = " name;
  print_proj (t, s, g);
  Printf.printf " (expected ";
  print_proj (et, es, eg);
  Printf.printf "): %s\n" (if pass then "OK" else "FAIL")

let () =
  (* Test zero *)
  Printf.printf "=== zero ===\n";
  test_project "zero" App.zero ("", "", 0);

  (* Test combine *)
  Printf.printf "\n=== combine ===\n";
  test_project "combine zero zero"
    (App.combine App.zero App.zero)
    ("", "fail", 0);
  test_project "combine (\"web\", \"success\", 90) (\"app\", \"success\", 80)"
    (App.combine ("web", "success", 90) ("app", "success", 80))
    ("webapp", "success", 85);
  test_project "combine (\"web\", \"success\", 90) (\"app\", \"fail\", 60)"
    (App.combine ("web", "success", 90) ("app", "fail", 60))
    ("webapp", "fail", 75);
  test_project "combine (\"a\", \"success\", 80) (\"b\", \"success\", 80)"
    (App.combine ("a", "success", 80) ("b", "success", 80))
    ("ab", "success", 80);
  test_project "combine (\"a\", \"fail\", 79) (\"b\", \"fail\", 79)"
    (App.combine ("a", "fail", 79) ("b", "fail", 79))
    ("ab", "fail", 79);
  test_project "combine (\"x\", \"success\", 100) (\"y\", \"success\", 100)"
    (App.combine ("x", "success", 100) ("y", "success", 100))
    ("xy", "success", 100);

  (* Test fail *)
  Printf.printf "\n=== fail ===\n";
  test_project "fail zero" (App.fail App.zero) ("", "fail", 0);
  test_project "fail (\"web\", \"success\", 90)"
    (App.fail ("web", "success", 90))
    ("web", "fail", 0);
  test_project "fail (\"app\", \"fail\", 50)"
    (App.fail ("app", "fail", 50))
    ("app", "fail", 0);

  (* Test success *)
  Printf.printf "\n=== success ===\n";
  test_project "success zero" (App.success App.zero) ("", "success", 80);
  test_project "success (\"web\", \"fail\", 0)"
    (App.success ("web", "fail", 0))
    ("web", "success", 80);
  test_project "success (\"app\", \"success\", 100)"
    (App.success ("app", "success", 100))
    ("app", "success", 80)
