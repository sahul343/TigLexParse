functor ExprLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Expr_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(*#line 1.2 "tiger.grm"*)(* This is the preamble where you can have arbitrary sml code. For us
it is empty *)


(*#line 15.1 "tiger.grm.sml"*)
end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\008\000\008\000\007\000\010\000\006\000\012\000\005\000\000\000\
\\001\000\002\000\015\000\003\000\014\000\004\000\013\000\005\000\012\000\
\\006\000\011\000\000\000\
\\001\000\002\000\015\000\003\000\014\000\004\000\013\000\005\000\012\000\
\\011\000\024\000\000\000\
\\001\000\006\000\010\000\007\000\009\000\000\000\
\\001\000\007\000\000\000\000\000\
\\027\000\000\000\
\\028\000\000\000\
\\029\000\001\000\008\000\008\000\007\000\010\000\006\000\012\000\005\000\000\000\
\\030\000\000\000\
\\031\000\000\000\
\\032\000\009\000\016\000\000\000\
\\033\000\004\000\013\000\005\000\012\000\000\000\
\\034\000\004\000\013\000\005\000\012\000\000\000\
\\035\000\000\000\
\\036\000\000\000\
\\037\000\002\000\015\000\003\000\014\000\004\000\013\000\005\000\012\000\000\000\
\\038\000\000\000\
\\039\000\000\000\
\"
val actionRowNumbers =
"\007\000\003\000\001\000\010\000\
\\000\000\016\000\009\000\006\000\
\\005\000\007\000\000\000\000\000\
\\000\000\000\000\000\000\002\000\
\\008\000\014\000\013\000\012\000\
\\011\000\015\000\017\000\004\000"
val gotoT =
"\
\\001\000\002\000\002\000\001\000\003\000\023\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\015\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\002\000\002\000\016\000\000\000\
\\001\000\017\000\000\000\
\\001\000\018\000\000\000\
\\001\000\019\000\000\000\
\\001\000\020\000\000\000\
\\001\000\021\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 24
val numrules = 14
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit | IDENTIFIER of  (string) | CONST of  (int) | PROGRAM of  (Ast.Expr list) | EXPS of  (Ast.Expr list) | EXP of  (Ast.Expr)
end
type svalue = MlyValue.svalue
type result = Ast.Expr list
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 6) => true | _ => false
val showTerminal =
fn (T 0) => "CONST"
  | (T 1) => "PLUS"
  | (T 2) => "MINUS"
  | (T 3) => "MUL"
  | (T 4) => "DIV"
  | (T 5) => "NEWLINE"
  | (T 6) => "EOF"
  | (T 7) => "IF"
  | (T 8) => "ASSIGN"
  | (T 9) => "LPAREN"
  | (T 10) => "RPAREN"
  | (T 11) => "IDENTIFIER"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.EXPS EXPS, EXPS1left, EXPS1right)) :: rest671)) => let val  result = MlyValue.PROGRAM ((*#line 49.33 "tiger.grm"*) EXPS (*#line 184.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, EXPS1left, EXPS1right), rest671)
end
|  ( 1, ( ( _, ( _, _, NEWLINE1right)) :: ( _, ( MlyValue.EXPS EXPS, EXPS1left, _)) :: rest671)) => let val  result = MlyValue.PROGRAM ((*#line 50.26 "tiger.grm"*) EXPS (*#line 188.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, EXPS1left, NEWLINE1right), rest671)
end
|  ( 2, ( ( _, ( _, _, EOF1right)) :: ( _, ( MlyValue.EXPS EXPS, EXPS1left, _)) :: rest671)) => let val  result = MlyValue.PROGRAM ((*#line 51.30 "tiger.grm"*)EXPS(*#line 192.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, EXPS1left, EOF1right), rest671)
end
|  ( 3, ( rest671)) => let val  result = MlyValue.EXPS ((*#line 53.33 "tiger.grm"*) []                  (*#line 196.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 4, ( ( _, ( MlyValue.EXPS EXPS, _, EXPS1right)) :: _ :: ( _, ( MlyValue.EXP EXP, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXPS ((*#line 54.26 "tiger.grm"*) EXP :: EXPS         (*#line 200.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, EXP1left, EXPS1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.CONST CONST, CONST1left, CONST1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 56.33 "tiger.grm"*) Ast.Const CONST     (*#line 204.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, CONST1left, CONST1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER, IDENTIFIER1left, IDENTIFIER1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 57.24 "tiger.grm"*) Ast.Var   IDENTIFIER(*#line 208.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, IDENTIFIER1left, IDENTIFIER1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 58.33 "tiger.grm"*) Ast.plus  EXP1 EXP2 (*#line 212.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 59.26 "tiger.grm"*) Ast.minus EXP1 EXP2 (*#line 216.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 60.26 "tiger.grm"*) Ast.mul   EXP1 EXP2 (*#line 220.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 61.30 "tiger.grm"*)Ast.divide  EXP1 EXP2(*#line 224.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, IDENTIFIER1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 62.27 "tiger.grm"*)Ast.assign (Ast.Var IDENTIFIER) EXP(*#line 228.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, IDENTIFIER1left, EXP1right), rest671)
end
|  ( 12, ( ( _, ( _, IF1left, IF1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 63.16 "tiger.grm"*)Ast.IF(*#line 232.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, IF1left, IF1right), rest671)
end
|  ( 13, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 64.25 "tiger.grm"*)EXP(*#line 236.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.PROGRAM x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : Expr_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun CONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(ParserData.MlyValue.CONST i,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(ParserData.MlyValue.VOID,p1,p2))
fun MUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(ParserData.MlyValue.VOID,p1,p2))
fun NEWLINE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(ParserData.MlyValue.VOID,p1,p2))
fun IDENTIFIER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(ParserData.MlyValue.IDENTIFIER i,p1,p2))
end
end