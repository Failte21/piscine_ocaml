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

let is_perfect = function
  | Nil -> true
  | tree ->
    let rec is_perfect_aux node =
    match node with
      | Node (_, Nil, Nil) -> true
      | Node (_, Nil, _) -> false
      | Node (_, _, Nil) -> false
      | Node (_, left, right) -> (is_perfect_aux left) && (is_perfect_aux right)
      | Nil -> true in
  (is_perfect_aux tree) && (is_bst tree)

let is_balanced = function
| Nil -> true
| tree ->
  let rec is_balanced_aux node acc =
  match node with
    | Node (_, Nil, Nil) -> true
    | Node (_, Nil, right) -> acc && (is_balanced_aux right false)
    | Node (_, left, Nil) -> acc && (is_balanced_aux left false)
    | Node (_, left, right) -> (is_balanced_aux left acc) && (is_balanced_aux right acc)
    | Nil -> true in
  (is_balanced_aux tree true) && (is_bst tree)

let rec search_bst tree searched_value =
  match tree with
    | Nil -> false
    | Node (value, left, right) when value = searched_value -> true
    | Node (_, left, right) -> (search_bst left searched_value) || (search_bst right searched_value)


let rec add_bst to_insert tree =
  match tree with
    | Node (value, left, right) when to_insert < value -> Node(value, (add_bst to_insert left), right)
    | Node (value, left, right) when to_insert > value -> Node(value, left, (add_bst to_insert right))
    | Node (value, _, _) -> failwith "BST Tree cannot have duplicated values"
    | Nil -> Node (to_insert, Nil, Nil)

let bst_trees = [
  let f = Node (65, Nil, Nil) in
  let e = Node (70, f, Nil) in
  let d = Node (30, Nil, Nil) in
  let c = Node (60, Nil, e) in
  let b = Node (40, d, Nil) in
  let a = Node (50, b, c) in
  a
]

let balanced_trees = [
  let e = Node (70, Nil, Nil) in
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

let all_trees = bst_trees @ balanced_trees @ perfect_trees

let main () =
  List.iter (fun tree -> if (is_bst tree) then print_endline "Tree is bst" else print_endline "Tree is not bst") bst_trees;
  List.iter (fun tree -> if (is_perfect tree) then print_endline "Tree is perfect" else print_endline "Tree is not perfect") bst_trees;
  List.iter (fun tree -> if (is_perfect tree) then print_endline "Tree is perfect" else print_endline "Tree is not perfect") perfect_trees;
  List.iter (fun tree -> if (is_balanced tree) then print_endline "Tree is balanced" else print_endline "Tree is not balanced") bst_trees;
  List.iter (fun tree -> if (search_bst tree 70) then print_endline "70 is in tree" else print_endline "70 is not in tree") all_trees;
  List.iter (fun tree -> if (is_balanced tree) then print_endline "Tree is balanced" else print_endline "Tree is not balanced") balanced_trees;

  let balanced_tree_a = (add_bst 45 (List.hd bst_trees)) in
  let balanced_tree_b = (add_bst 55 balanced_tree_a) in
  let balanced_tree_c = (add_bst 55 balanced_tree_b) in
  if (is_balanced balanced_tree_a) then print_endline "Tree is balanced" else print_endline "Tree is not balanced";
  if (is_balanced balanced_tree_b) then print_endline "Tree is balanced" else print_endline "Tree is not balanced"

let () = main ()