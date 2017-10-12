fun OperateTo low high =
	if(low > high)
	then([])
	else(low::OperateTo (low+1) high)

fun NotDivisable r ns [] = List.map(fn x => OperateTo(Int.max(1,(x-r))) (x+r)) ns
|	NotDivisable r ns (q::qs) = 
		List.map( List.filter(fn y => y mod q <> 0)) (NotDivisable r ns qs)


fun FromTo low high =
	if low > high then [] else low ::(FromTo (low+1) high)

fun NotDivisible2 r Ns [] = List.map (fn m  => FromTo (Int.max(1,(m-r))) (m+r)) Ns
|   NotDivisible2 r Ns (q::qs) = List.map (List.filter (fn y => y mod q <> 0)) (NotDivisible2 r Ns qs)
(*foldr::op [] l*)

(*fun NotDivisible r  Ns   []   = List.map (fn m  => FromTo (Int.max(1,(m-r))) (m+r)) Ns|   NotDivisible r  Ns   (q   ::   qs) =List.map (List.filter (fn y  =>   y mod q <>   0)) (NotDivisible r  Ns qs)*)