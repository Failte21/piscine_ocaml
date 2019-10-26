(* return: ’a -> ’a Set.t
Creates a singleton containing the value given in argument.
• bind: ’a Set.t -> (’a -> ’b Set.t) -> ’b Set.t
Applies the function to every element in the Set and returns a new set.
• union: ’a Set.t -> ’a Set.t -> ’a Set.t Returns a new set containing the
union of the two sets given in argument.
• inter: ’a Set.t -> ’a Set.t -> ’a Set.t
Returns a new set containing the intersection of the two sets given in argument.
• diff: ’a Set.t -> ’a Set.t -> ’a Set.t Returns a new set containing the
difference between the two sets given in argument.
• filter: ’a Set.t -> (’a -> bool) -> ’a Set.t
Returns a new set containing only the elements that satisfy the predicate given in
argument.
• foreach: ’a Set.t -> (’a -> unit) -> unit
Executes the function given in argument on every element in the Set.
15
OCaml Pool - d09 Monoids and Monads also known as The Day the student stood still
• for_all: ’a Set.t -> (’a -> bool) -> bool
Returns true if all the elements in the set satisfy the predicate given in argument,
false otherwise.
• exists: ’a Set.t -> (’a -> bool) -> bool Returns true if at least one element in the set satisfies the predicate given in argument, false otherwise. *)

module Set =
struct
  type 'a t = 'a list
  let return a = [a]
  let bind elem fn = List.map fn elem

  let union la lb =
    let rec union_aux l acc =
      match l with
        | h::t ->
          begin
            let filtered = List.filter (fun e -> e <> h) t in
            union_aux filtered (acc @ [h])
          end
        | [] -> acc in
      union_aux (la @ lb) []

  let inter la lb =
    let rec inter_aux l acc =
      match l with
        | h::t ->
          begin
            let val_maybe = List.find_opt (fun e -> e = h) t in
            match val_maybe with
              | Some e -> inter_aux t (acc @ [h])
              | _ -> inter_aux t acc
          end
        | [] -> acc in
      inter_aux (la @ lb) []

  let diff la lb =
    let rec diff_aux la lb acc =
      match la with
        | h::t ->
          begin
            let val_maybe = List.find_opt (fun e -> e = h) lb in
            match val_maybe with
              | None -> diff_aux t lb (acc @ [h])
              | _ -> diff_aux t lb acc
          end
        | [] -> acc in
      (diff_aux la lb []) @ (diff_aux lb la [])

  let filter elem predicate = List.filter predicate elem
  let foreach elem fn = List.iter fn elem
  let for_all elem predicate = List.for_all predicate elem
  let exists elem predicate = List.exists predicate elem
end