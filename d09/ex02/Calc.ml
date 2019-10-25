module Calc =
functor (M: Arithmetic.MONOID) ->
struct
  let add x y = M.add x y
  let sub x y = M.sub x y
  let div x y = M.div x y
  let mul x y = M.mul x y
  let power x n =
    let rec power_pos x n acc =
      if n = 0 then acc
      else power_pos x (n - 1) (mul acc x) in
    let rec power_neg x n acc =
      if n = 0 then acc
      else power_neg x (n + 1) (div acc x) in
    if n < 0 then power_neg x n M.zero2 else power_pos x n M.zero2
  let fact x =
    let rec fact_aux x acc =
      if x <= M.zero1 then acc else fact_aux (sub x M.zero2) (mul acc x) in
    if x < M.zero1 then invalid_arg "Does not support negative value"
    else fact_aux x M.zero2
end

module CalcInt = Calc(Int.INT)
module CalcFloat = Calc(Float.FLOAT)