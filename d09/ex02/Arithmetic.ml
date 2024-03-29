module type MONOID =
sig
  type element
  val zero1: element
  val zero2: element
  val add: element -> element -> element
  val sub: element -> element -> element
  val mul: element -> element -> element
  val div: element -> element -> element
end