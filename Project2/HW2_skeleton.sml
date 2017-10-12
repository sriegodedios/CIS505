(*
   The Interpret function assumes a "parse" function,
      written in another file.
*)

(* ABSTRACT SYNTAX

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
| ErrorP of string
*)
use "HW2_parser.sml";
(* EXCEPTIONS *)
exception ProcedureNotDeclared of string;
(* *** PERHAPS ADD A BIT MORE *)
(* BASIC DEFINITIONS *)

type Val = int
  (* the values of expressions, and the values that can be stored *)

type ProcVal = Id * Comm
  (* the formal parameter, and the body, of the procedure *)

(* Global Environment For Functions and Procedures *)

type ProcEnv = (Id * ProcVal) list

(* PEnvInit: unit -> ProcEnv *)
fun PEnvInit () = []

(* PEnvInsert: Id -> ProcVal -> ProcEnv -> ProcEnv *)
fun PEnvInsert id dv penv = (id,dv) :: penv

(* PEnvLookup: ProcEnv -> Id -> ProcVal *)
fun PEnvLookup [] x = raise (ProcedureNotDeclared x)
|   PEnvLookup ((y,dv)::penv) x =
      if x = y then dv
      else PEnvLookup penv x

(* Stores *)

type GlobalStore = (Id * Val) list  (* for integer identifiers *)

type LocalStore = (Id * Val) list
   (* for a procedure's parameter, its auxiliary variables (if any),
        and for a special identifier "result"   *)

type Stores = GlobalStore * LocalStore option
   (* the local store only exists inside procedure calls *)

(* StoLookup: (Global/Local)Store -> Id -> Val option   *)
fun StoLookup [] _ = NONE
|   StoLookup ((x1,v1)::s1) x =
         if x = x1 then SOME v1 else StoLookup s1 x

(* StosLookup: Stores -> Id -> Val option
      first checks the local store   *)
fun StosLookup (global_sto, NONE) x = StoLookup global_sto x
|   StosLookup (global_sto, SOME local_sto) x =
(*#########################################################*)
    let val lookup = StoLookup local_sto x
    in
      case lookup of
      (* if the local store has it*)
      NONE => StoLookup global_sto x
      | _ => lookup
      (* if the global store has it*)
    end
                     (**** MODIFY ****)
(*#########################################################*)


(* StosUpdate: Loc -> Val -> Stores -> Stores *)
fun StosUpdate l v (global_sto, NONE) = ((l,v)::global_sto, NONE)
  (* if local store exists, updates the local store
       unless variable defined globally but not locally *)
|   StosUpdate l v (global_sto, SOME local_sto) =
(*#########################################################*)
        (*
          1) if its in local write local
          2) if its in global but not in local write it to global
          3) write it to local
        *)
        let val globallookup = StosLookup (global_sto, NONE) l
        in
          case globallookup of
          SOME _ =>
            let val locallookup = StoLookup local_sto l
            in
            (*Case if it exsist in the global lookup*)
            case locallookup of
              SOME _ => (global_sto, SOME ((l,v)::local_sto))
              | NONE => ((l,v)::global_sto, SOME local_sto)
            end
          | NONE => (global_sto, SOME ((l,v)::local_sto))
        end
                      (* *** MODIFY *)
(*#########################################################*)


(* EVALUATION OF EXPRESSIONS
     ExpEval: Exp -> Stores -> Val
*)

fun ExpEval (NumE n) _ = n
|   ExpEval (IdE id) stos = getOpt(StosLookup stos id, 0)
(*#########################################################*)

  (* let val lookup = StosLookup stos id
    in
      case lookup of
      SOME t => t
      | NONE => 0
    end*)



    (* *** MODIFY *)
(*#########################################################*)
|   ExpEval (AddE(e1,e2)) stos = (ExpEval e1 stos) + (ExpEval e2 stos) (* *** MODIFY *)
|   ExpEval (SubE(e1,e2)) stos = (ExpEval e1 stos) - (ExpEval e2 stos)(* *** MODIFY *)
|   ExpEval (MulE(e1,e2)) stos = (ExpEval e1 stos) * (ExpEval e2 stos) (* *** MODIFY *)

(* PROCESSING OF DECLARATIONS
     DeclExec: Decl -> ProcEnv -> ProcEnv
*)

fun DeclExec (ProcD (pid,fid,cmd)) penv =
       PEnvInsert pid (fid,cmd) penv
|   DeclExec (SeqD(decl1,decl2)) penv =
      let val penv1 = DeclExec decl1 penv
          val penv2 = DeclExec decl2 penv1
       in penv2
      end
|   DeclExec NoDecl penv = penv


(* EXECUTION OF COMMANDS *)

type InputStream = Num list
type OutputStream = Val list
type RunTimeState = Stores * InputStream * OutputStream

(*
CommExec: Comm -> ProcEnv -> RunTimeState -> RunTimeState
*)

fun CommExec SkipC penv state = state
|   CommExec (SeqC(cmd1,cmd2)) penv state =
      (*CommExec cmd1 penv state  (* *** MODIFY *)*)
(*###########################################################*)

      (*Do the command one first before doing command 2*)
      let
        val state1 = CommExec cmd1 penv state
      in CommExec cmd2 penv state1
      end
(*###########################################################*)

(*###########################################################
  First you evaluate the expression and check if it returns 0.
  If it returns 0, you want to excute the "else" statement.
  If it returns any other number, then you want to return the
  "then" statement
#################################################################*)


|   CommExec (IfC(exp,cmd1,cmd2)) penv (state as (stos,_,_)) =
      (*CommExec cmd1 penv state *)(* *** MODIFY *)

      if ExpEval exp stos = 0
      then(CommExec cmd2 penv state)(*It's false excute the second command*)
      else(CommExec cmd1 penv state) (*It's true excute the first command*)



|   CommExec (WhileC(exp,cmd)) penv state =
  (*Creates a new command
  Create an If c command
  Recursively CommExec (command your creating) pnev state*)
      CommE
      (*CommExec cmd penv state*) (* *** MODIFY *)
      if ExpVal exp sto = 0 (*Some how get it as a tuple*)
      then(state)
      else(CommExec smd penv state)



(*|   CommExec (AssignC(id,exp)) penv (stos, inp, outp) =
      let val v = ExpEval exp stos

       in ((StosUpdate stos id), inp, outp) (* *** MODIFY *)

      end*)
|   CommExec (OutputC exp) penv (stos,inp,outp) =
      let val v = ExpEval exp stos

       in (stos, inp, (v::outp))   (* we eventually reverse the order *)

      end
|   CommExec (InputC id) penv (stos,inp,outp) =
(*###########################################################*)
        ((StosUpdate id (hd inp) stos), (tl inp), outp)


        (* *** MODIFY *)
(*###########################################################*)
|   CommExec (ProcCallC (pid,exp)) penv
                (stos as (global_sto,local_sto), inp, outp) =
      let val v = ExpEval exp stos
          val (formal,body) = PEnvLookup penv pid
          val call_stores = StosLookup (global_sto, local_sto) formal

          (* *** MODIFY *)
        in CommExec body penv (call_stores, inp, outp) (* *** MOFIFY *)
      end
|   CommExec (FunCallC (id,pid,exp)) penv
                (stos as (global_sto,local_sto), inp, outp) =P
      let val v = ExpEval exp stos
          val (formal,body) = PEnvLookup penv pid
          val call_stores = (global_sto, local_sto) (* *** MODIFY *)
          val (stos' as (global_sto', local_sto'), inp', outp') =
                 CommExec body penv (call_stores, inp, outp)
        in (StosUpdate id 47 (* *** MODIFY *)
              (global_sto,local_sto), inp, outp)  (* *** MODIFY *)
      end

(* RUNNING THE PROGRAM *)

fun ProgRun (ProgP(decl,comm)) inp =
       let val penv = DeclExec decl (PEnvInit())
           val (_,_,outp) = CommExec comm penv (([], NONE), inp, [])
         in rev outp
        end
|   ProgRun(ErrorP s) _ = (print ("*** syntax error: "^s^"\n"); [0])

fun Interpret prog inp = ProgRun (parse prog) inp
      handle
        (ProcedureNotDeclared x) =>
            (print ("*** error: "^x^" used as procedure but is not declared\n"); [0])
    (* *** HANDLE MORE EXCEPTIONS? *)
