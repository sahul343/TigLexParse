functor TigerLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Tiger_TOKENS
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
\\001\000\001\000\011\000\016\000\010\000\020\000\009\000\022\000\008\000\
\\025\000\007\000\027\000\006\000\028\000\005\000\030\000\004\000\000\000\
\\001\000\002\000\024\000\003\000\023\000\004\000\022\000\005\000\021\000\
\\006\000\020\000\007\000\019\000\008\000\018\000\009\000\017\000\
\\010\000\016\000\011\000\015\000\012\000\014\000\013\000\013\000\
\\015\000\012\000\000\000\
\\001\000\002\000\024\000\003\000\023\000\004\000\022\000\005\000\021\000\
\\006\000\020\000\007\000\019\000\008\000\018\000\009\000\017\000\
\\010\000\016\000\011\000\015\000\012\000\014\000\013\000\013\000\
\\018\000\052\000\000\000\
\\001\000\002\000\024\000\003\000\023\000\004\000\022\000\005\000\021\000\
\\006\000\020\000\007\000\019\000\008\000\018\000\009\000\017\000\
\\010\000\016\000\011\000\015\000\012\000\014\000\013\000\013\000\
\\021\000\051\000\000\000\
\\001\000\002\000\024\000\003\000\023\000\004\000\022\000\005\000\021\000\
\\006\000\020\000\007\000\019\000\008\000\018\000\009\000\017\000\
\\010\000\016\000\011\000\015\000\012\000\014\000\013\000\013\000\
\\024\000\050\000\000\000\
\\001\000\002\000\024\000\003\000\023\000\004\000\022\000\005\000\021\000\
\\006\000\020\000\007\000\019\000\008\000\018\000\009\000\017\000\
\\010\000\016\000\011\000\015\000\012\000\014\000\013\000\013\000\
\\024\000\067\000\000\000\
\\001\000\002\000\024\000\003\000\023\000\004\000\022\000\005\000\021\000\
\\006\000\020\000\007\000\019\000\008\000\018\000\009\000\017\000\
\\010\000\016\000\011\000\015\000\012\000\014\000\013\000\013\000\
\\026\000\062\000\000\000\
\\001\000\002\000\024\000\003\000\023\000\004\000\022\000\005\000\021\000\
\\011\000\080\000\012\000\080\000\015\000\080\000\017\000\080\000\
\\018\000\080\000\021\000\080\000\024\000\080\000\026\000\080\000\
\\029\000\080\000\031\000\080\000\032\000\080\000\033\000\080\000\000\000\
\\001\000\002\000\024\000\003\000\023\000\004\000\022\000\005\000\021\000\
\\011\000\081\000\012\000\081\000\015\000\081\000\017\000\081\000\
\\018\000\081\000\021\000\081\000\024\000\081\000\026\000\081\000\
\\029\000\081\000\031\000\081\000\032\000\081\000\033\000\081\000\000\000\
\\001\000\002\000\024\000\003\000\023\000\004\000\022\000\005\000\021\000\
\\011\000\082\000\012\000\082\000\015\000\082\000\017\000\082\000\
\\018\000\082\000\021\000\082\000\024\000\082\000\026\000\082\000\
\\029\000\082\000\031\000\082\000\032\000\082\000\033\000\082\000\000\000\
\\001\000\002\000\024\000\003\000\023\000\004\000\022\000\005\000\021\000\
\\011\000\083\000\012\000\083\000\015\000\083\000\017\000\083\000\
\\018\000\083\000\021\000\083\000\024\000\083\000\026\000\083\000\
\\029\000\083\000\031\000\083\000\032\000\083\000\033\000\083\000\000\000\
\\001\000\002\000\024\000\003\000\023\000\004\000\022\000\005\000\021\000\
\\011\000\084\000\012\000\084\000\015\000\084\000\017\000\084\000\
\\018\000\084\000\021\000\084\000\024\000\084\000\026\000\084\000\
\\029\000\084\000\031\000\084\000\032\000\084\000\033\000\084\000\000\000\
\\001\000\002\000\024\000\003\000\023\000\004\000\022\000\005\000\021\000\
\\011\000\085\000\012\000\085\000\015\000\085\000\017\000\085\000\
\\018\000\085\000\021\000\085\000\024\000\085\000\026\000\085\000\
\\029\000\085\000\031\000\085\000\032\000\085\000\033\000\085\000\000\000\
\\001\000\015\000\000\000\000\000\
\\001\000\019\000\049\000\000\000\
\\001\000\019\000\055\000\000\000\
\\001\000\028\000\029\000\000\000\
\\001\000\028\000\047\000\000\000\
\\001\000\031\000\046\000\000\000\
\\001\000\032\000\059\000\000\000\
\\070\000\000\000\
\\071\000\001\000\011\000\016\000\010\000\020\000\009\000\022\000\008\000\
\\025\000\007\000\027\000\006\000\028\000\005\000\030\000\004\000\000\000\
\\072\000\002\000\024\000\003\000\023\000\004\000\022\000\005\000\021\000\
\\006\000\020\000\007\000\019\000\008\000\018\000\009\000\017\000\
\\010\000\016\000\011\000\015\000\012\000\014\000\013\000\013\000\
\\029\000\060\000\000\000\
\\073\000\000\000\
\\074\000\000\000\
\\075\000\019\000\028\000\000\000\
\\076\000\000\000\
\\077\000\000\000\
\\078\000\004\000\022\000\005\000\021\000\000\000\
\\079\000\004\000\022\000\005\000\021\000\000\000\
\\086\000\002\000\024\000\003\000\023\000\004\000\022\000\005\000\021\000\
\\006\000\020\000\007\000\019\000\008\000\018\000\009\000\017\000\
\\010\000\016\000\013\000\013\000\000\000\
\\087\000\002\000\024\000\003\000\023\000\004\000\022\000\005\000\021\000\
\\006\000\020\000\007\000\019\000\008\000\018\000\009\000\017\000\
\\010\000\016\000\011\000\015\000\013\000\013\000\000\000\
\\088\000\002\000\024\000\003\000\023\000\004\000\022\000\005\000\021\000\
\\006\000\020\000\007\000\019\000\008\000\018\000\009\000\017\000\
\\010\000\016\000\011\000\015\000\012\000\014\000\013\000\013\000\000\000\
\\089\000\000\000\
\\090\000\002\000\024\000\003\000\023\000\004\000\022\000\005\000\021\000\
\\006\000\020\000\007\000\019\000\008\000\018\000\009\000\017\000\
\\010\000\016\000\011\000\015\000\012\000\014\000\013\000\013\000\000\000\
\\091\000\002\000\024\000\003\000\023\000\004\000\022\000\005\000\021\000\
\\006\000\020\000\007\000\019\000\008\000\018\000\009\000\017\000\
\\010\000\016\000\011\000\015\000\012\000\014\000\013\000\013\000\000\000\
\\092\000\002\000\024\000\003\000\023\000\004\000\022\000\005\000\021\000\
\\006\000\020\000\007\000\019\000\008\000\018\000\009\000\017\000\
\\010\000\016\000\011\000\015\000\012\000\014\000\013\000\013\000\000\000\
\\093\000\002\000\024\000\003\000\023\000\004\000\022\000\005\000\021\000\
\\006\000\020\000\007\000\019\000\008\000\018\000\009\000\017\000\
\\010\000\016\000\011\000\015\000\012\000\014\000\013\000\013\000\
\\017\000\063\000\000\000\
\\094\000\000\000\
\\095\000\000\000\
\\096\000\033\000\027\000\000\000\
\\097\000\000\000\
\\098\000\002\000\024\000\003\000\023\000\004\000\022\000\005\000\021\000\
\\006\000\020\000\007\000\019\000\008\000\018\000\009\000\017\000\
\\010\000\016\000\011\000\015\000\012\000\014\000\013\000\013\000\000\000\
\"
val actionRowNumbers =
"\000\000\001\000\040\000\025\000\
\\038\000\016\000\000\000\000\000\
\\000\000\024\000\020\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\040\000\
\\018\000\017\000\000\000\014\000\
\\004\000\003\000\002\000\008\000\
\\031\000\030\000\012\000\011\000\
\\009\000\007\000\010\000\027\000\
\\026\000\029\000\028\000\041\000\
\\021\000\015\000\032\000\000\000\
\\000\000\033\000\000\000\019\000\
\\022\000\000\000\006\000\034\000\
\\037\000\039\000\021\000\042\000\
\\000\000\000\000\023\000\005\000\
\\036\000\000\000\035\000\013\000"
val gotoT =
"\
\\001\000\001\000\003\000\067\000\000\000\
\\000\000\
\\004\000\024\000\005\000\023\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\028\000\000\000\
\\001\000\029\000\000\000\
\\001\000\030\000\000\000\
\\000\000\
\\000\000\
\\001\000\031\000\000\000\
\\001\000\032\000\000\000\
\\001\000\033\000\000\000\
\\001\000\034\000\000\000\
\\001\000\035\000\000\000\
\\001\000\036\000\000\000\
\\001\000\037\000\000\000\
\\001\000\038\000\000\000\
\\001\000\039\000\000\000\
\\001\000\040\000\000\000\
\\001\000\041\000\000\000\
\\001\000\042\000\000\000\
\\004\000\043\000\005\000\023\000\000\000\
\\000\000\
\\000\000\
\\001\000\046\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\052\000\002\000\051\000\000\000\
\\000\000\
\\000\000\
\\001\000\054\000\000\000\
\\001\000\055\000\000\000\
\\000\000\
\\001\000\056\000\000\000\
\\000\000\
\\000\000\
\\001\000\059\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\052\000\002\000\062\000\000\000\
\\000\000\
\\001\000\063\000\000\000\
\\001\000\064\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\066\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 68
val numrules = 29
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
datatype svalue = VOID | ntVOID of unit | IDENTIFIER of  (string) | CONST of  (int) | DEC of  (Ast.Dec) | DECS of  (Ast.Dec list) | PROGRAM of  (Ast.Expr) | EXPS of  (Ast.Expr list) | EXP of  (Ast.Expr)
end
type svalue = MlyValue.svalue
type result = Ast.Expr
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
fn (T 14) => true | _ => false
val showTerminal =
fn (T 0) => "CONST"
  | (T 1) => "PLUS"
  | (T 2) => "MINUS"
  | (T 3) => "MUL"
  | (T 4) => "DIV"
  | (T 5) => "LT"
  | (T 6) => "EQ"
  | (T 7) => "LTEQ"
  | (T 8) => "GTEQ"
  | (T 9) => "GT"
  | (T 10) => "AND"
  | (T 11) => "OR"
  | (T 12) => "NTEQ"
  | (T 13) => "NEWLINE"
  | (T 14) => "EOF"
  | (T 15) => "IF"
  | (T 16) => "ELSE"
  | (T 17) => "THEN"
  | (T 18) => "ASSIGN"
  | (T 19) => "LPAREN"
  | (T 20) => "RPAREN"
  | (T 21) => "WHILE"
  | (T 22) => "UMINUS"
  | (T 23) => "DO"
  | (T 24) => "FOR"
  | (T 25) => "TO"
  | (T 26) => "BREAK"
  | (T 27) => "IDENTIFIER"
  | (T 28) => "SEMICOLON"
  | (T 29) => "LET"
  | (T 30) => "IN"
  | (T 31) => "END"
  | (T 32) => "VAR"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( _, _, EOF1right)) :: ( _, ( MlyValue.EXP EXP, EXP1left, _)) :: rest671)) => let val  result = MlyValue.PROGRAM ((*#line 67.36 "tiger.grm"*) EXP (*#line 341.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, EXP1left, EOF1right), rest671)
end
|  ( 1, ( rest671)) => let val  result = MlyValue.EXPS ((*#line 69.33 "tiger.grm"*) []                  (*#line 345.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 2, ( ( _, ( MlyValue.EXP EXP, EXP1left, EXP1right)) :: rest671)) => let val  result = MlyValue.EXPS ((*#line 70.18 "tiger.grm"*) EXP :: []         (*#line 349.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, EXP1left, EXP1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.EXPS EXPS, _, EXPS1right)) :: _ :: ( _, ( MlyValue.EXP EXP, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXPS ((*#line 71.28 "tiger.grm"*) EXP :: EXPS         (*#line 353.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, EXP1left, EXPS1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.CONST CONST, CONST1left, CONST1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 73.33 "tiger.grm"*) Ast.Const CONST     (*#line 357.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, CONST1left, CONST1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER, IDENTIFIER1left, IDENTIFIER1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 74.24 "tiger.grm"*) Ast.Var   IDENTIFIER(*#line 361.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, IDENTIFIER1left, IDENTIFIER1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 75.26 "tiger.grm"*) Ast.mul   EXP1 EXP2 (*#line 365.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 76.30 "tiger.grm"*)Ast.divide  EXP1 EXP2(*#line 369.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 77.30 "tiger.grm"*) Ast.plus  EXP1 EXP2 (*#line 373.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 78.26 "tiger.grm"*) Ast.minus EXP1 EXP2 (*#line 377.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 79.25 "tiger.grm"*)Ast.equal  EXP1 EXP2(*#line 381.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 80.27 "tiger.grm"*)Ast.notequal  EXP1 EXP2(*#line 385.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 81.27 "tiger.grm"*)Ast.lessequal  EXP1 EXP2(*#line 389.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 82.25 "tiger.grm"*)Ast.less  EXP1 EXP2(*#line 393.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 83.27 "tiger.grm"*)Ast.greatequal  EXP1 EXP2(*#line 397.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 84.25 "tiger.grm"*)Ast.great  EXP1 EXP2(*#line 401.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 85.26 "tiger.grm"*)Ast.logicaland  EXP1 EXP2(*#line 405.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 86.25 "tiger.grm"*)Ast.or  EXP1 EXP2(*#line 409.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, IDENTIFIER1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 87.27 "tiger.grm"*)Ast.assign (Ast.Var IDENTIFIER) EXP(*#line 413.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, IDENTIFIER1left, EXP1right), rest671)
end
|  ( 19, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 88.25 "tiger.grm"*)EXP(*#line 417.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, WHILE1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 89.26 "tiger.grm"*)Ast.While EXP1 EXP2(*#line 421.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, WHILE1left, EXP2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.EXP EXP3, _, EXP3right)) :: _ :: ( _, ( MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, _, _)) :: ( _, ( _, FOR1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 90.45 "tiger.grm"*)Ast.For (Ast.Var IDENTIFIER) EXP1 EXP2 EXP3(*#line 425.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, FOR1left, EXP3right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.EXP EXP3, _, EXP3right)) :: _ :: ( _, ( MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 91.30 "tiger.grm"*)Ast.Closedif EXP1 EXP2 EXP3(*#line 429.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, IF1left, EXP3right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 92.23 "tiger.grm"*)Ast.Openif EXP1 EXP2 (*#line 433.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, IF1left, EXP2right), rest671)
end
|  ( 24, ( ( _, ( _, BREAK1left, BREAK1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 93.13 "tiger.grm"*)Ast.BREAK	(*#line 437.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 25, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.EXPS EXPS, _, _)) :: _ :: ( _, ( MlyValue.DECS DECS, _, _)) :: ( _, ( _, LET1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 94.27 "tiger.grm"*)Ast.LET (DECS, EXPS) (*#line 441.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, LET1left, END1right), rest671)
end
|  ( 26, ( rest671)) => let val  result = MlyValue.DECS ((*#line 95.23 "tiger.grm"*)[](*#line 445.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, defaultPos, defaultPos), rest671)
end
|  ( 27, ( ( _, ( MlyValue.DECS DECS, _, DECS1right)) :: ( _, ( MlyValue.DEC DEC, DEC1left, _)) :: rest671)) => let val  result = MlyValue.DECS ((*#line 96.14 "tiger.grm"*)DEC :: DECS	(*#line 449.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, DEC1left, DECS1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 98.34 "tiger.grm"*)Ast.VarDec( (Ast.Var IDENTIFIER) , EXP) (*#line 453.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, VAR1left, EXP1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.PROGRAM x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : Tiger_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun CONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(ParserData.MlyValue.CONST i,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(ParserData.MlyValue.VOID,p1,p2))
fun MUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(ParserData.MlyValue.VOID,p1,p2))
fun LTEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(ParserData.MlyValue.VOID,p1,p2))
fun GTEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(ParserData.MlyValue.VOID,p1,p2))
fun NTEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(ParserData.MlyValue.VOID,p1,p2))
fun NEWLINE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(ParserData.MlyValue.VOID,p1,p2))
fun UMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(ParserData.MlyValue.VOID,p1,p2))
fun TO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(ParserData.MlyValue.VOID,p1,p2))
fun IDENTIFIER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(ParserData.MlyValue.IDENTIFIER i,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(ParserData.MlyValue.VOID,p1,p2))
end
end
