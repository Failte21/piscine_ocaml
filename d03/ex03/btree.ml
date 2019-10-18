(* Allowed functions : Pervasives module and List.hd *)

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let is_bst = function
  | Nil -> true
  | Node (v, l, r) ->
    let rec is_bst_aux node ref_val op =
      match node with
      | Node (value, left, right) ->
        (op value ref_val) &&
        (is_bst_aux right value (>)) &&
        (is_bst_aux left value (<))
      | Nil -> true in
    (is_bst_aux r v (>)) && (is_bst_aux l v (<))

let rec is_perfect = function
  | Node (_, Nil, Nil) -> true
  | Node (_, Nil, _) -> false
  | Node (_, _, Nil) -> false
  | Node (_, left, right) -> (is_perfect left) && (is_perfect right)
  | Nil -> true

let bst_trees = [
  let f = Node (65, Nil, Nil) in
  let e = Node (70, f, Nil) in
  let d = Node (30, Nil, Nil) in
  let c = Node (60, Nil, e) in
  let b = Node (40, d, Nil) in
  let a = Node (50, b, c) in
  a
]

let perfect_trees = [
  let c = Node (60, Nil, Nil) in
  let b = Node (40, Nil, Nil) in
  let a = Node (50, b, c) in
  a
]

let main () =
  List.iter (fun tree -> if (is_bst tree) then print_endline "Tree is bst" else print_endline "Tree is not bst") bst_trees;
  List.iter (fun tree -> if (is_perfect tree) then print_endline "Tree is perfect" else print_endline "Tree is not perfect") bst_trees;
  List.iter (fun tree -> if (is_perfect tree) then print_endline "Tree is perfect" else print_endline "Tree is not perfect") perfect_trees

let () = main ()