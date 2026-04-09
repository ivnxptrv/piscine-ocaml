type project = string * string * int

let zero = ("", "", 0)

let combine p1 p2 =
  let type1, status1, grade1 = p1 in
  let type2, status2, grade2 = p2 in
  let avgGrade = (grade1 + grade2) / 2 in
  let status = if avgGrade >= 80 then "success" else "fail" in
  (type1 ^ type2, status, avgGrade)

let fail p =
  let t, _, _ = p in
  (t, "fail", 0)

let success p =
  let t, _, _ = p in
  (t, "success", 80)
