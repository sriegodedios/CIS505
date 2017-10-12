
fun change(_, 0)=[]
  | change([],_) = raise Div
  | change(coins as c::cs, a) =
      if(c>a)
      then(change(cs,a))
      else(c::change(coins, a-c))
