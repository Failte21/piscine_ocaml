(* Allowed functions : Graphics module and Pervasives module *)

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

(* The size of a binary tree is the number of nodes that are defined *)
(* val size : ’a tree -> int *)
let rec size = function
  | Node(v, a, b) -> 1 + (size a) + (size b)
  | Nil -> 0

(* The height of
a binary tree is the number of relations on the longest downward path between the root
and a leaf. *)

(* val height : ’a tree -> int *)
let height tree =
  let rec height_aux tree acc =
    match tree with
      | Node(v, a, b) -> max (height_aux a (acc + 1)) (height_aux b (acc + 1))
      | Nil -> acc in
    height_aux tree 0
  
(* val draw_tree : string tree -> unit *)

let main () =
  let f = Node (42, Nil, Nil) in
  let e = Node (42, f, Nil) in
  let d = Node (42, Nil, Nil) in
  let c = Node (42, Nil, e) in
  let b = Node (42, d, Nil) in
  let tree = Node (42, b, c) in
  let tree_size = size tree in
  let tree_height = height tree in
  print_string "size: ";
  print_int tree_size;
  print_char '\n';
  print_string "height: ";
  print_int tree_height;
  print_char '\n'

let () = main ()