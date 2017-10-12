datatype 'a gtree  =
    Leaf of 'a
| Node of 'a branches and 'a branches =
    Empty
| Branch of 'a gtree * 'a branches
(*
  acL = aculimator
  acJ = operation to execute (function)
  acN = aculimator for the nodes
  acE = aculimator for empty
*)

fun display (Leaf x) = (Int.toString x)^" "
|   display (Node bs) = case display_b bs of
       "" => ""
     | s  => "[ "^s^"] "
and display_b Empty = ""
|   display_b (Branch(t,bs)) = (display t)^(display_b bs)

val tree1 = Node(Branch(Leaf(7),
                 Branch(Node(Branch(Leaf(5),Branch(Leaf(4),Empty))),
                 Branch(Node(Empty),Empty))))
(*************************************************************************)
(*Map function*)
fun map f (Leaf(n)) = Leaf(f(n))
  | map f (Node(bs)) =
      Node(map_branches f bs)
and map_branches f (Empty) = Empty
  | map_branches f (Branch(gt, bs)) =
    Branch(map f gt, map_branches f bs)
(*************************************************************************)
(*Fold function*)
fun fold _ acL _ _ (Leaf x) = acL x
  | fold acJ acL acN acE (Node bs) =
    let
      val r = fold_b acJ acL acN acE bs
      in acN r
    end
and fold_b _ _ _ acE Empty = acE
  | fold_b acJ acL acN acE (Branch(t, bs)) =
    let
      val r1 = fold acJ acL acN acE t
      val r2 = fold_b acJ acL acN acE bs
    in acJ(r1, r2)
    end
(*************************************************************************)
val id = fn x => x
val sum' = fold op+ id id 0
val prod' = fold op* id id 1
val subtr' = fold op- id id 0
val treeprod = prod' tree1
val display' = fold op^(fn x => (Int.toString x)^"_") (fn x => if x = "" then "" else "[_"^x^"]_")"";

display'  (map (fn x => x*2) tree1)








(*display' prod'*)
