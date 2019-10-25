module type APP =
sig
  type project = string * string * int
  val zero : project
  val combine : project -> project -> project
  val fail : project -> project
  val success : project -> project
end

let average a b = (a + b) / 2

module App: APP =
struct
  type project = string * string * int
  let zero = ("", "", 0)
  let combine (s_a,_, grade_a) (s_b, _, grade_b) =
    let grade = (grade_a + grade_b) / 2 in
    let status = if grade < 80 then "fail" else "success" in
    (s_a ^ s_b, status, grade)
  let success (s, _, _) = (s, "success", 80)
  let fail (s, _, _) = (s, "fail", 0)
end