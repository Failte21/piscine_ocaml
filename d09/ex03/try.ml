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