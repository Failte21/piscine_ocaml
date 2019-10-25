module Watchtower =
struct
  type hour = int
  let zero = 12
  
  let reborn hour = if hour = 0 then zero else hour

  let pos hour =
    if hour <= 0 then zero + (hour mod zero) else hour

  let add a b = reborn ((pos (a + b)) mod zero)
  let sub a b = reborn ((pos (a - b)) mod zero)
end