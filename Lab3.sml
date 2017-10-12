(* Author: Shane Riegodedios *)

datatype 'a gtree = Leaf of 'a
  | Node of 'a branches
and 'a branches = Empty
  | Branch of 'a gtree * 'a branches

val tree1 = Node
  (Branch(Leaf(7),
   Branch(Node(Branch(Leaf(5),
               Branch(Leaf(4), Empty))),
   Branch(Node(Empty), Empty))))


(* The display function that displays the tree *)
fun display (Leaf x) =
       (Int.toString x)^"_"
   | display (Node bs) =
       case display_b bs of
       "" => ""
       | s => " [_"^s^"]_"
   and display_b Empty = ""
   | display_b (Branch(t,bs)) =
  (display t)^(display_b bs)

(* Product function of the tree *)
fun prod (Leaf n) = n
    | prod (Node bs) = branch_prod(bs)
and branch_prod (Empty) = 1
    | branch_prod (Branch(gt, bs)) =
        prod(gt) * branch_prod(bs)
(* Fold function for the tree *)
fun fold f acL acN e (Leaf x) = acL x
  | fold f acL acN e (Node(bs)) = acN (fold_b f acL acN e bs)
and fold_b f acL acN e (Empty) = e
  | fold_b f acL acN e (Branch(gt, bs)) =
      f((fold f acL acN e gt), (fold_b f acL acN e bs));


val prod' = fold op* (fn x => x)(fn x => x)1;

val display' = fold op^(fn x => (Int.toString x)^"_") (fn x => if x = "" then "" else "[_"^x^"]_")"";

prod' tree1;

display' tree1;
