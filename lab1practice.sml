fun Factors(N,low,high) =
	if(low < high)
	then(
		if(N mod low = 0)
		then(low::Factors(N,low+1,high))
		else(Factors(N,low+1,high))	
		)
	else([]);