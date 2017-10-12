(*Author: Shane Riegodedios
* CIS 505
* Torbuen Antov
**)

(* General Functions *)

exception InputError of string

(***Basic Functions to create dictionary****)
fun updateF key v dict = fn k =>
  if key = k then v else dict k

fun lookupF dict key = dict key

fun power10 p =  (* computes 10^p for p >= 0 *)
   if p = 0
   then 1.0
   else(10.0 * power10(p-1)) (* MODIFY *)

fun is_digit c = #"0" <= c andalso c <= #"9"

fun digit2int c =
      let val i = ord(c) - ord(#"0")
       in if 0 <= i andalso i <= 9
          then i
          else raise InputError "digit expected"
      end

fun get_nat cs =  (* takes a list of characters and
                       converts the leading digits into an integer which
                       is returned together with the remaining characters *)
  let fun gn acc [] = (acc,[])
      |   gn acc (cs as (c :: cs')) =
              if is_digit c
              then gn (10 * acc + digit2int c) cs'
              else (acc,cs)
   in case cs of
        [] => raise InputError "input exhausted though digit was expected"
      | (c :: cs') => if is_digit c
                   then gn (digit2int c) cs'
                   else raise InputError "digit was expected"
  end

fun get_real cs = (* takes a list of characters and
                       converts the leading digits into a real which
                       is returned together with the remaining characters *)
  let val (m,cs1) = get_nat cs
   in case cs1 of
       [] => (real(m), [])
     | (c1 :: cs') =>
       if c1 = #"E"
       then let val (p,cs2) = get_nat cs'
             in ((real(m) * (power10 p)), cs2)
            end
       else if c1 = #"e"
       then let val (p,cs2) = get_nat cs'
            in ((real(m) / (power10 p)), cs2)
            end (* MODIFY *)
      else (real(m), cs1)
      end

(* Simulation *)

(* 'sim cs stk regs'  returns the result of simulating
     the character sequence 'cs' on stack 'stk' and registers 'regs'
*)
fun sim [] stk regs = (* all input has been processed; now output stack *) stk

|   sim (cs as (c1 :: cs')) stk regs =
   if c1 = #" " then (* ignore blanks *) sim cs' stk regs
   else
   if is_digit c1   (* expect a real and put it on the top of the stack *)
     then let val (r,cs'') = get_real cs
           in sim cs'' (r :: stk) regs
          end
   else
   if c1 = #"+" orelse c1 = #"-" orelse c1 = #"*" orelse c1 = #"/"
     (* special handling of binary operators *)
     then sim_binop c1 cs' stk regs
   else
   if c1 = #"~" (* special handling of unary operator *)
     then sim_unop c1 cs' stk regs
   else
   if c1 = #"R"    (* expect the index of a register to be Read *)
     then let val (i,cs'') = get_nat cs'
           in sim_read i cs'' stk regs
          end
   else
   if c1 = #"W"    (* expect the index of a register to be Written *)
     then let val (i,cs'') = get_nat cs'
     in sim_write i cs'' stk regs
     end (* MODIFY - 9/13/2017*)
   else
   if c1 = #"A" (* pop all stack elements, add them and push the result *)
     then if List.map(fn n => round(n)) stk = [] (*first check if stack is empty *) (* MODIFY *)
          then sim cs' (0.0::stk) regs (*Just add the number 0 to the stack*)
          else if List.map(fn n => round(n))  (tl(stk)) = nil
          then sim cs' stk regs
          else if List.map(fn n => round(n)) (tl(tl(stk))) = nil (*If there is only one element in the list*)
          then sim cs' ((hd(stk)+hd(tl(stk)))::(tl(tl(stk)))) regs
          else sim cs ((hd(stk)+hd(tl(stk)))::(tl(tl(stk)))) regs
   else
   if c1 = #"C" (* expect a real by which to multiply all stack elements,
                    and all register values,
                        so as to Change the unit of measure *)
     then let val (i, s) = get_real cs'
          in sim s (List.map(fn x => x*i) stk) (fn j => i * (regs(j)))
          end (* MODIFY *)
   else raise InputError ("unknown symbol")

and sim_binop opr cs (v2 :: v1 :: stk) regs =
     (* pop the two topmost stack elements, apply the binary operator,
        and push the result on the stack *)
        if opr = #"+"
        then sim cs ((v2 + v1) :: stk) regs
        else if opr = #"-"
        then sim cs ((v1 - v2) :: stk) regs
        else if opr = #"*"
        then sim cs ((v2 * v1) :: stk) regs
        else if opr = #"/"
        then sim cs ((v1 / v2) :: stk) regs
        else raise InputError("unknown symbol !!!")
         (* MODIFY *)

(* WRITE CLAUSES FOR WHEN THE STACK HAS LESS THAN TWO ELEMENTS *)

and sim_unop opr cs (v1 :: stk) regs =
      sim cs (~v1 :: stk) regs (* MODIFY *)


and sim_read i cs stk regs = (* the content of register 'i' is put on stack *)
      sim cs ((lookupF regs i)::stk) regs (* MODIFY *)
      (*sim cs stk regs*)


and sim_write i cs (v :: stk) regs =
   (* pop the top of the stack and write it to register 'i' *)
      (*First check if the stack is empty*)
    sim cs stk (updateF i v regs)
    | sim_write i cs nil regs =
      raise InputError("Not enough items in stack")

      (*sim cs (v::stk) regs*)




    (*sim cs stk regs*)








       (* MODIFY *)
(* WRITE CLAUSE FOR EMPTY STACK *)

(* top-level *)

fun simulate inp =
     sim
       (explode inp)
       []
       (fn i => raise InputError

          ("nothing stored in register "^(Int.toString i)))
      handle (InputError x) => (print x; print"\n"; []);



simulate "5 7+"; (*[12.0]*)
simulate "25e1 7 3- *"; (*[10.0]*)
simulate "5~7 +"; (*[2.0]*)
simulate "75e1 3 /"; (*[2.5]*)

simulate "5 7 * W12 20 R12 + R12 +"; (*[90.0]*)
simulate "5 7 2 *"; (*[14.0,5.0]*)
simulate "2 3 4 6 A"; (*[15.0]*)
simulate "A"; (*[0.0]*)
simulate "2 6 3 W4 C1E2 + R4"; (*[300.0,800.0]*)
