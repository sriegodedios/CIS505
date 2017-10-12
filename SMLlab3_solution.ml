datatype 'a gtree  =
   Leaf of 'a
 | Node of 'a branches
and 'a branches =
   Empty
 | Branch of 'a gtree * 'a branches

(*
         /\
        /  \
       /    \
      12    17
*)

val tree0 = Node(Branch(Leaf(12),Branch(Leaf(17),Empty)))

(*
         /|\
        / | \
       /  |  \ 
      7  / \ 
        /   \
       /     \ 
      5       4
*)
val tree1 = 
   Node(Branch(Leaf(7),
        Branch(Node(Branch(Leaf(5),Branch(Leaf(4),Empty))),
        Branch(Node(Empty),
          Empty))))

fun sum (Leaf x) = x
|   sum (Node bs) = sum_b bs
and sum_b Empty = 0
|   sum_b (Branch(t,bs)) = sum t + sum_b bs

val tree0sum = sum tree0 (* = 29 *)
val tree1sum = sum tree1 (* = 16 *)

fun prod (Leaf x) = x
|   prod (Node bs) = prod_b bs
and prod_b Empty = 1
|   prod_b (Branch(t,bs)) = prod t * prod_b bs

val tree0prod = prod tree0 (* = 204 *)
val tree1prod = prod tree1 (* = 140 *)

fun display (Leaf x) = (Int.toString x)^" "
|   display (Node bs) = case display_b bs of
       "" => ""
     | s  => "[ "^s^"] "
and display_b Empty = ""
|   display_b (Branch(t,bs)) = (display t)^(display_b bs)

val tree0display = display tree0 (* = "[ 12 17 ] " *)
val tree1display = display tree1 (* = "[ 7 [ 5 4 ] ] " *)

fun fold _ acL _ _ (Leaf x) = acL x
|   fold acJ acL acN acE (Node bs) =
      let val r = fold_b acJ acL acN acE bs
       in acN r
      end
and fold_b _ _ _ acE Empty = acE
|   fold_b acJ acL acN acE (Branch (t,bs)) =
      let val r1 = fold   acJ acL acN acE t
          val r2 = fold_b acJ acL acN acE bs
       in acJ (r1,r2) end

val id = fn x => x

val sum' = fold op+ id id 0

val prod' = fold op* id id 1

val display' = 
   fold op^ 
       (fn n => (Int.toString n)^" ") 
       (fn s => if s = "" then s else "[ " ^s^"] ") 
       ""

val tree0sum' = sum' tree0 (* = 29 *)
val tree1sum' = sum' tree1 (* = 16 *)
val tree0prod' = prod' tree0 (* = 204 *)
val tree1prod' = prod' tree1 (* = 140 *)
val tree0display' = display' tree0 (* = "[ 12 17 ] " *)
val tree1display' = display' tree1 (* = "[ 7 [ 5 4 ] ] " *)
