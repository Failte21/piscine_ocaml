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

let rec min_bst = function
  | Node (value, Nil, Nil) -> value
  | Node (value, left, Nil) -> min_bst left
  | Node (value, _, right) -> min_bst right
  | Nil -> 0

let delete_min tree =
  let rec delete_min_aux node acc =
    match node with
      | Nil -> acc
      | Node (value, Nil, Nil) -> acc
      | Node (value, left, Nil) -> delete_min_aux left (Node (value, left, Nil))
      | Node (value, left, right) -> delete_min_aux right (Node (value, left, right)) in
    delete_min_aux tree Nil

let rec delete_bst to_delete tree =
  match tree with
    | Node (value, Nil, Nil) when to_delete = value -> Nil
    | Node (value, Nil, Nil) when to_delete = value -> Nil
    | Node (value, left, right) when to_delete < value -> Node (value, right, (delete_bst to_delete left))
    | Node (value, left, right) when to_delete > value -> Node (value, (delete_bst to_delete right), left)
    | Node (value, left, Nil) when to_delete = value -> left
    | Node (value, left, right) when to_delete = value -> Node ((min_bst right), left, delete_min right)
    | Node (value, _, _) -> failwith "BST Tree cannot have duplicated values"
    | Nil -> Node (to_delete, Nil, Nil)

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

let tree_names_values = [
  ("a", 42);
  ("b", 40);
  ("c", 50);
  ("d", 30);
  ("e", 41);
  ("f", 43);
  ("g", 60);
  ("h", 70);
  ("i", 80);
]

let green = "\x1b[32m"
let reset = "\x1b[0m"

let rec test_delete current_tree current_name deleted_value remaining_value =
  Printf.printf "%s-----DELETE %d FROM TREE %s-----%s\n" green deleted_value current_name reset ;
  let new_tree = delete_bst deleted_value current_tree in
  if (is_perfect new_tree) then print_endline "Tree is perfect\n" else print_endline "Tree is not perfect\n";
  if (is_balanced new_tree) then print_endline "Tree is balanced\n" else print_endline "Tree is not balanced\n";
  if (search_bst new_tree deleted_value) then Printf.printf "%d is in tree\n" deleted_value else Printf.printf "%d is not in tree\n" deleted_value;
  if (search_bst new_tree remaining_value) then Printf.printf "%d is in tree\n" remaining_value else Printf.printf "%d is not in tree\n" remaining_value;
  print_char '\n'

let rec test_create tree_names_values current_tree current_name =
  Printf.printf "%s-----TEST WITH TREE %s-----%s\n" green current_name reset;
  if (is_perfect current_tree) then print_endline "Tree is perfect\n" else print_endline "Tree is not perfect\n";
  if (is_balanced current_tree) then print_endline "Tree is balanced\n" else print_endline "Tree is not balanced\n";
  if (search_bst current_tree 60) then print_endline "60 is in tree\n" else print_endline "60 is not in tree\n";
  match tree_names_values with
    | (name, value)::t ->
      let new_name = if current_name = "Nil" then name else current_name ^ name in
      let new_tree = add_bst value current_tree in
      test_create t new_tree new_name
    | _ -> test_delete current_tree current_name 60 80

let main () =
  test_create tree_names_values Nil "Nil"

let () = main ()