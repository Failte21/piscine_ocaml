module Calc =
functor (M: Arithmetic.MONOID) ->
struct
  let add x y = M.add x y
  let sub x y = M.sub x y
  let div x y = M.div x y
  let mul x y = M.mul x y
end

module CalcInt = Calc(Int.INT)
module CalcFloat = Calc(Float.FLOAT)