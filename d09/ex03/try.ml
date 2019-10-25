(* In this exercise you will implement a monad named Try, to provide a more functional
and elegant way to handle exceptions. An instance of Try can be either of:
• Success of ’a
• Failure of exn
Your monad module will implement the following functions:
• return: ’a -> ’a Try.t
Creates a Success which contains your value.
• bind: ’a Try.t -> (’a -> ’b Try.t) -> ’b Try.t
Applies a function to your monad, converting it to a Failure if your function
argument raises an exception. Your function is only applied if your monad is a
Success.
• recover: ’a Try.t -> (exn -> ’a Try.t) -> ’a Try.t
If your monad is a Failure, applies the function to it.
• filter: ’a Try.t -> (’a -> bool) -> ’a Try.t
Converts your monad to a Failure if your monad is a Success that does not satisfy
the predicate given in argument.
13
OCaml Pool - d09 Monoids and Monads also known as The Day the student stood still
• flatten: ’a Try.t Try.t -> ’a Try.t
Flattens a nested Try into a simple Try. Note that a Success of Failure is a
Failure. *)

exception FilterException of string

module Try =
struct
  type 'a t =
    | Success of 'a
    | Failure of exn
  let bind elem fn =
    match elem with
      | Success e ->
        begin
          try
            fn e
          with
            err -> Failure err
        end
      | failure -> failure
  let return elem = Success elem
  let recover elem fn =
    match elem with
      | Failure err ->
        begin
          try
            fn err
          with
            err -> Failure err
        end
      | failure -> failure
  let filter elem fn =
    match elem with
      | Success e -> if fn e then Success e else Failure (FilterException "¯\\_(ツ)_/¯")
      | failure -> failure
  let flatten elem =
    match elem with
      | Success s ->
        begin
          match s with
            | Success e -> Success e
            | failure -> failure
        end
      | Failure e -> Failure e
end