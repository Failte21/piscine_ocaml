module INT =
struct
  type element = int
  let zero1 = 0
  let zero2 = 1
  let add element_a element_b =
    zero1 + element_a + element_b
  let sub element_a element_b =
    zero1 - element_a - element_b
  let mul element_a element_b =
    zero2 * element_a * element_b
  let div element_a element_b =
    zero2 * element_a / element_b
end