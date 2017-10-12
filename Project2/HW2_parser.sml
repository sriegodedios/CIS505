(*

Language grammar

P : Program
C : Command
E : Expression
D : Declaration
N : Numeral
F,I: Identifier

P ::=  D  C

C ::=  skip |  C1 ; C2  |  if E { C1 } { C2 }  |  while E { C }  
   |  I = E   |  output E  |  input I
   |  call F E  |  I = call F E   

E ::=   N  |  I  |  (E1 + E2)  |  (E1 * E2)  |  (E1 - E2)

D ::=  procedure F ( I ) { C } ; D2 
   |  <nothing>

N ::=  strings of digits

I ::=  strings of letters or digits, starting with a letter, 
           not including keywords

*)

(* --- type declarations, for forming operator trees (abstract syntax) --- *)

type Num = int

type Id = string

datatype Exp =
  NumE of Num
| IdE of Id
| AddE of Exp * Exp
| SubE of Exp * Exp
| MulE of Exp * Exp

datatype Comm =
  SkipC
| SeqC of Comm * Comm
| IfC of Exp * Comm * Comm
| WhileC of Exp * Comm
| AssignC of Id * Exp
| OutputC of Exp
| InputC of Id
| ProcCallC of Id * Exp 
| FunCallC of Id * Id * Exp

datatype Decl = 
  ProcD of Id * Id * Comm
| SeqD of Decl * Decl
| NoDecl

datatype Prog = 
  ProgP of Decl * Comm
| ErrorP of string   (* to report errors *)

(* ============= scanner ================= *)
(* the scanner converts the input into a list of "tokens" *)

datatype Token = 
   SkipT
 | IfT
 | WhileT
 | OutputT
 | InputT
 | CallT
 | ProcDecT
 | SemicT
 | EqualT
 | PlusT
 | MinusT
 | TimesT
 | LparenT
 | RparenT
 | LcurlyT
 | RcurlyT
 | IdT of string
 | NumT of int

(* --- auxiliary functions --- *)

fun print_token token = case token of
   SkipT => "skip"
 | IfT => "if"
 | WhileT => "while"
 | OutputT => "output"
 | InputT => "input"
 | CallT => "call"
 | ProcDecT => "procedure"
 | SemicT => ";"
 | EqualT => "="
 | PlusT => "+"
 | MinusT => "-"
 | TimesT => "*"
 | LparenT => "("
 | RparenT => ")"
 | LcurlyT => "{"
 | RcurlyT => "}"
 | (IdT s) => ("identifier "^s)
 | (NumT n) => "number"

fun is_digit(ch) = ord(ch) >= ord(#"0") andalso ord(ch) <= ord(#"9")

fun char2digit(ch) = ord(ch) - ord(#"0")

fun is_letter(ch) = 
      (ord(ch) >= ord(#"a") andalso ord(ch) <= ord(#"z"))
         orelse
      (ord(ch) >= ord(#"A") andalso ord(ch) <= ord(#"Z"))

(* scanNum: char list -> (int * char list) *)
fun scanNum(inp) = 
      let fun scan([],acc) = (acc,[])
          |   scan(c::cs,acc) =
                if is_digit(c)
                then scan(cs,10 * acc + char2digit(c))
                else (acc,c::cs)
       in scan(inp,0)
      end

(* scanId: char list -> (string * char list) *)
fun scanId(inp) =
      let fun scan([],acc) = (acc,[]) 
          |   scan(cs as c::cs', acc) = 
                if is_letter(c) orelse is_digit(c)
                then scan(cs', acc^(implode [c]))
                else (acc,cs)
       in scan(inp,"") 
      end

(* scan: char list -> Token list *)
fun scan [] = []
|   scan (cs as (c::cs1)) =
      if is_digit(c)
      then let val (n,cs2) = scanNum(cs) 
            in (NumT n) :: (scan cs2) 
           end  
      else if c = #";"
           then SemicT :: (scan cs1)
      else if c = #"="
           then EqualT :: (scan cs1)
      else if c = #"+"
           then PlusT :: (scan cs1)
      else if c = #"-"
           then MinusT :: (scan cs1)
      else if c = #"*"
           then TimesT :: (scan cs1)
      else if c = #"("
           then LparenT :: (scan cs1)
      else if c = #")"
           then RparenT :: (scan cs1)
      else if c = #"{"
           then LcurlyT :: (scan cs1)
      else if c = #"}"
           then RcurlyT :: (scan cs1)
      else if is_letter(c)
      then let val (s,cs2) = scanId(cs)
            in if s = "skip"
               then SkipT :: (scan cs2)
               else if s = "if"
               then IfT :: (scan cs2)
               else if s = "while"
               then WhileT :: (scan cs2)
               else if s = "output"
               then OutputT :: (scan cs2)
               else if s = "input"
               then InputT :: (scan cs2)
               else if s = "call"
               then CallT :: (scan cs2)
               else if s = "procedure"
               then ProcDecT :: (scan cs2)
               else (IdT s) :: (scan cs2)
           end
      else scan cs1

(* ============= parser =========== *)

exception SyntaxError of string

(* expectToken(token,token::tokens) = tokens   *)
fun expectToken(token,[]) = 
      raise (SyntaxError ((print_token token)^" expected"))
|   expectToken(token,token1::tokens) =
      if token = token1
      then tokens 
      else raise (SyntaxError  
        ((print_token token)^" expected but "^(print_token token1)^" seen"))

fun getIdT((IdT s) :: tokens) = (s,tokens)
|   getIdT([]) = 
      raise (SyntaxError "identifier expected")
|   getIdT(token :: tokens) = raise (SyntaxError 
       ("identifier expected but "^(print_token token)^" seen"))

(* parseExp: tokens -> Exp * tokens   *)
fun parseExp([]) = raise (SyntaxError "expression expected")
|   parseExp((NumT n) :: tokens) = (NumE n,tokens)
|   parseExp((IdT s) :: tokens) = (IdE s,tokens)
|   parseExp(LparenT :: tokens) =
      let val (exp1,tokens1) = parseExp(tokens)
       in case tokens1 of
            (PlusT :: tokens0) =>
               (let val (exp2,tokens2) = parseExp(tokens0)
                 in (AddE(exp1,exp2),expectToken(RparenT,tokens2))
                end)
          | (MinusT :: tokens0) =>
               (let val (exp2,tokens2) = parseExp(tokens0)
                 in (SubE(exp1,exp2),expectToken(RparenT,tokens2))
                end)
          | (TimesT :: tokens0) =>
               (let val (exp2,tokens2) = parseExp(tokens0)
                 in (MulE(exp1,exp2),expectToken(RparenT,tokens2))
                end)
          | otherwise => raise (SyntaxError "operator expected")
      end
|   parseExp(token :: tokens) = 
      raise (SyntaxError ((print_token token)^" cannot start an expression"))

(* parse1Comm: tokens -> Comm * tokens       *)
(*  reads a command that is "atomic, i.e.,   *)
(*   not formed by sequential composition    *)
fun parse1Comm([]) = raise (SyntaxError "command expected")
|   parse1Comm(SkipT :: tokens) =
           (SkipC, tokens)
|   parse1Comm(IfT :: tokens) =
      let val (exp,tokens1) = parseExp(tokens)
          val (comm1,tokens2) = parseComm(expectToken(LcurlyT,tokens1))
          val (comm2,tokens3) = parseComm(expectToken(LcurlyT,
                                    expectToken(RcurlyT,tokens2)))
       in (IfC(exp,comm1,comm2),expectToken(RcurlyT,tokens3))
      end
|   parse1Comm(WhileT :: tokens) =
      let val (exp,tokens1) = parseExp(tokens)
          val (comm,tokens2) = parseComm(expectToken(LcurlyT,tokens1))
       in (WhileC(exp,comm),expectToken(RcurlyT,tokens2))
      end
|   parse1Comm(OutputT :: tokens) =
      let val (exp,tokens1) = parseExp(tokens)
       in (OutputC(exp),tokens1)
      end
|   parse1Comm(InputT :: tokens) =
      let val (s,tokens1) = getIdT(tokens)
       in (InputC(s),tokens1)
      end
|   parse1Comm(CallT :: tokens) =
      let val (s,tokens1) = getIdT(tokens)
          val (exp,tokens2) = parseExp(tokens1)
       in (ProcCallC(s,exp),tokens2)
      end
|   parse1Comm((IdT s) :: tokens) =
      let val tokens1 = expectToken(EqualT,tokens) in
         case tokens1 of
           (CallT :: tokens2) =>
              (let val (s1,tokens3) = getIdT(tokens2)
                   val (exp,tokens4) = parseExp(tokens3)
                in (FunCallC(s,s1,exp),tokens4)
               end)
         | _ => (let val (exp,tokens2) = parseExp(tokens1)
                  in (AssignC(s,exp),tokens2)
                 end)
       end
|   parse1Comm(token :: tokens) = 
      raise (SyntaxError ((print_token token)^" cannot start command"))
(* parseComm: tokens -> Comm * tokens                *)
(*  reads a sequence of one or more atomic  commands *)
and parseComm(tokens) = 
      let val (comm1,tokens1) = parse1Comm(tokens)
       in case tokens1 of
            (SemicT :: tokens2) => 
                  (let val (comm2,tokens3) = parseComm(tokens2)
                    in (SeqC(comm1,comm2),tokens3)
                   end)
          | otherwise => (comm1,tokens1)
      end

(* parse1Decl: tokens -> Decl * tokens      
     reads what comes after 'procedure' in a procedure declaration *)
fun parse1Decl tokens =
      let val (s,tokens1) = getIdT(tokens)
          val (s1,tokens2) = getIdT(expectToken(LparenT,tokens1))
          val (comm,tokens3) = 
                 parseComm(expectToken(LcurlyT,expectToken(RparenT,tokens2)))
       in (ProcD(s,s1,comm),expectToken(SemicT,expectToken(RcurlyT,tokens3)))
      end

(* parseProg: tokens -> Prog * tokens   *)
fun parseProg tokens = case tokens of
      [] => raise (SyntaxError "a program must contain a command")
    | (token :: tokens1) => 
          (if token = ProcDecT
           then let 
               val (decl1,tokens2) = parse1Decl tokens1
               val (ProgP(decls,comm),tokens3) = parseProg tokens2
              in (ProgP(SeqD(decl1,decls),comm),tokens3)
             end
           else let val (comm,tokens2) = parseComm tokens
                   in (ProgP(NoDecl,comm),tokens2)
                  end)

(* parse: string -> Prog *)
fun parse inp =
      (let val tokens = scan (explode inp)
           val (prog,tokens1) = parseProg tokens
        in if null tokens1
           then prog
           else raise (SyntaxError "input contains symbols after the program")
      end)
      handle (SyntaxError s) => ErrorP s