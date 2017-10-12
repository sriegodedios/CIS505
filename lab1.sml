(*
/**
* Author: Shane Riegodedios, Shawn Kirby
* CIS 505
* Lab 1
**/

*)

(*
This function takes a number and 2 ranges and returns a list of all the 
Factors in that range.
*)
fun Factors(N, low, high) =
if low > high
(*checks if the range of the lower is greater than the high*)
then([]) (*Makes an empty list*)
else(if N mod low = 0 (*Checks to see if the remainer is 0*)
then(low :: Factors(N, low + 1, high))
else(Factors(N, low + 1, high (*Recursively calls the method*))));

(* This function finds the Greatest Common Denominator*) 
fun SharedFactors(x, y) =
if x = 0 orelse y = 0
(*Handles the 0 case*)
then(0)
else(if x = y
then(x)
else(if x > y 
then(SharedFactors(x-y, y))
else(SharedFactors(x, y-x))
)
);


(*

This function takes in two numbers and sees if it's a 
Co prime.

*) 
fun CoPrime(N,M) =
	if SharedFactors(N, M) = 1 
	then(true)
	else(false);