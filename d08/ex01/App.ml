type status = Fail | Succeed

type project = string * string * int

let zero : project = ("", "", 0)

let first (x, _, _) = x

let second (_, y, _) = y

let third (_, _, z) = z

let combine p1 p2 =
  let description = first p1 ^ first p2 in
  let average_grade = (third p1 + third p2) / 2 in
  let status = if average_grade >= 80 then "succeed" else "fail" in
  (description, status, average_grade)

let fail p = (first p, "fail", 0)

let success p = (first p, "succeed", 80)
