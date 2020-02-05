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
\\001\000\001\000\013\000\003\000\012\000\016\000\011\000\020\000\010\000\
\\022\000\009\000\024\000\008\000\026\000\007\000\027\000\006\000\
\\029\000\005\000\000\000\
\\001\000\001\000\036\000\000\000\
\\001\000\002\000\028\000\003\000\027\000\004\000\026\000\005\000\025\000\
\\006\000\024\000\007\000\023\000\008\000\022\000\009\000\021\000\
\\010\000\020\000\011\000\019\000\012\000\018\000\013\000\017\000\
\\018\000\058\000\000\000\
\\001\000\002\000\028\000\003\000\027\000\004\000\026\000\005\000\025\000\
\\006\000\024\000\007\000\023\000\008\000\022\000\009\000\021\000\
\\010\000\020\000\011\000\019\000\012\000\018\000\013\000\017\000\
\\021\000\057\000\000\000\
\\001\000\002\000\028\000\003\000\027\000\004\000\026\000\005\000\025\000\
\\006\000\024\000\007\000\023\000\008\000\022\000\009\000\021\000\
\\010\000\020\000\011\000\019\000\012\000\018\000\013\000\017\000\
\\023\000\056\000\000\000\
\\001\000\002\000\028\000\003\000\027\000\004\000\026\000\005\000\025\000\
\\006\000\024\000\007\000\023\000\008\000\022\000\009\000\021\000\
\\010\000\020\000\011\000\019\000\012\000\018\000\013\000\017\000\
\\023\000\074\000\000\000\
\\001\000\002\000\028\000\003\000\027\000\004\000\026\000\005\000\025\000\
\\006\000\024\000\007\000\023\000\008\000\022\000\009\000\021\000\
\\010\000\020\000\011\000\019\000\012\000\018\000\013\000\017\000\
\\025\000\069\000\000\000\
\\001\000\002\000\028\000\003\000\027\000\004\000\026\000\005\000\025\000\
\\006\000\024\000\007\000\023\000\008\000\022\000\009\000\021\000\
\\010\000\020\000\011\000\019\000\012\000\018\000\013\000\017\000\
\\035\000\059\000\000\000\
\\001\000\002\000\028\000\003\000\027\000\004\000\026\000\005\000\025\000\
\\011\000\088\000\012\000\088\000\015\000\088\000\017\000\088\000\
\\018\000\088\000\021\000\088\000\023\000\088\000\025\000\088\000\
\\028\000\088\000\030\000\088\000\031\000\088\000\032\000\088\000\
\\035\000\088\000\000\000\
\\001\000\002\000\028\000\003\000\027\000\004\000\026\000\005\000\025\000\
\\011\000\089\000\012\000\089\000\015\000\089\000\017\000\089\000\
\\018\000\089\000\021\000\089\000\023\000\089\000\025\000\089\000\
\\028\000\089\000\030\000\089\000\031\000\089\000\032\000\089\000\
\\035\000\089\000\000\000\
\\001\000\002\000\028\000\003\000\027\000\004\000\026\000\005\000\025\000\
\\011\000\090\000\012\000\090\000\015\000\090\000\017\000\090\000\
\\018\000\090\000\021\000\090\000\023\000\090\000\025\000\090\000\
\\028\000\090\000\030\000\090\000\031\000\090\000\032\000\090\000\
\\035\000\090\000\000\000\
\\001\000\002\000\028\000\003\000\027\000\004\000\026\000\005\000\025\000\
\\011\000\091\000\012\000\091\000\015\000\091\000\017\000\091\000\
\\018\000\091\000\021\000\091\000\023\000\091\000\025\000\091\000\
\\028\000\091\000\030\000\091\000\031\000\091\000\032\000\091\000\
\\035\000\091\000\000\000\
\\001\000\002\000\028\000\003\000\027\000\004\000\026\000\005\000\025\000\
\\011\000\092\000\012\000\092\000\015\000\092\000\017\000\092\000\
\\018\000\092\000\021\000\092\000\023\000\092\000\025\000\092\000\
\\028\000\092\000\030\000\092\000\031\000\092\000\032\000\092\000\
\\035\000\092\000\000\000\
\\001\000\002\000\028\000\003\000\027\000\004\000\026\000\005\000\025\000\
\\011\000\093\000\012\000\093\000\015\000\093\000\017\000\093\000\
\\018\000\093\000\021\000\093\000\023\000\093\000\025\000\093\000\
\\028\000\093\000\030\000\093\000\031\000\093\000\032\000\093\000\
\\035\000\093\000\000\000\
\\001\000\015\000\000\000\000\000\
\\001\000\019\000\055\000\000\000\
\\001\000\019\000\062\000\000\000\
\\001\000\027\000\032\000\000\000\
\\001\000\027\000\037\000\000\000\
\\001\000\027\000\054\000\000\000\
\\001\000\030\000\053\000\000\000\
\\001\000\031\000\066\000\000\000\
\\077\000\002\000\028\000\003\000\027\000\004\000\026\000\005\000\025\000\
\\006\000\024\000\007\000\023\000\008\000\022\000\009\000\021\000\
\\010\000\020\000\011\000\019\000\012\000\018\000\013\000\017\000\000\000\
\\078\000\001\000\013\000\003\000\012\000\016\000\011\000\020\000\010\000\
\\022\000\009\000\024\000\008\000\026\000\007\000\027\000\006\000\
\\029\000\005\000\000\000\
\\079\000\002\000\028\000\003\000\027\000\004\000\026\000\005\000\025\000\
\\006\000\024\000\007\000\023\000\008\000\022\000\009\000\021\000\
\\010\000\020\000\011\000\019\000\012\000\018\000\013\000\017\000\
\\028\000\067\000\000\000\
\\080\000\000\000\
\\081\000\002\000\028\000\003\000\027\000\004\000\026\000\005\000\025\000\
\\006\000\024\000\007\000\023\000\008\000\022\000\009\000\021\000\
\\010\000\020\000\011\000\019\000\012\000\018\000\013\000\017\000\000\000\
\\082\000\002\000\028\000\003\000\027\000\004\000\026\000\005\000\025\000\
\\006\000\024\000\007\000\023\000\008\000\022\000\009\000\021\000\
\\010\000\020\000\011\000\019\000\012\000\018\000\013\000\017\000\000\000\
\\083\000\002\000\028\000\003\000\027\000\004\000\026\000\005\000\025\000\
\\006\000\024\000\007\000\023\000\008\000\022\000\009\000\021\000\
\\010\000\020\000\011\000\019\000\012\000\018\000\013\000\017\000\000\000\
\\084\000\002\000\028\000\003\000\027\000\004\000\026\000\005\000\025\000\
\\006\000\024\000\007\000\023\000\008\000\022\000\009\000\021\000\
\\010\000\020\000\011\000\019\000\012\000\018\000\013\000\017\000\
\\017\000\070\000\000\000\
\\085\000\002\000\028\000\003\000\027\000\004\000\026\000\005\000\025\000\
\\006\000\024\000\007\000\023\000\008\000\022\000\009\000\021\000\
\\010\000\020\000\011\000\019\000\012\000\018\000\013\000\017\000\000\000\
\\086\000\000\000\
\\087\000\000\000\
\\094\000\002\000\028\000\003\000\027\000\004\000\026\000\005\000\025\000\
\\006\000\024\000\007\000\023\000\008\000\022\000\009\000\021\000\
\\010\000\020\000\013\000\017\000\000\000\
\\095\000\002\000\028\000\003\000\027\000\004\000\026\000\005\000\025\000\
\\006\000\024\000\007\000\023\000\008\000\022\000\009\000\021\000\
\\010\000\020\000\011\000\019\000\013\000\017\000\000\000\
\\096\000\000\000\
\\097\000\000\000\
\\098\000\004\000\026\000\005\000\025\000\000\000\
\\099\000\004\000\026\000\005\000\025\000\000\000\
\\100\000\000\000\
\\101\000\000\000\
\\102\000\000\000\
\\103\000\019\000\016\000\034\000\015\000\036\000\014\000\000\000\
\\104\000\032\000\031\000\000\000\
\\105\000\000\000\
\\106\000\002\000\028\000\003\000\027\000\004\000\026\000\005\000\025\000\
\\006\000\024\000\007\000\023\000\008\000\022\000\009\000\021\000\
\\010\000\020\000\011\000\019\000\012\000\018\000\013\000\017\000\000\000\
\\107\000\000\000\
\\108\000\000\000\
\\109\000\000\000\
\"
val actionRowNumbers =
"\000\000\042\000\022\000\043\000\
\\046\000\031\000\017\000\000\000\
\\000\000\000\000\001\000\039\000\
\\018\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\043\000\
\\020\000\019\000\015\000\004\000\
\\003\000\002\000\041\000\047\000\
\\007\000\026\000\009\000\034\000\
\\033\000\013\000\012\000\010\000\
\\008\000\011\000\036\000\035\000\
\\038\000\037\000\044\000\023\000\
\\016\000\000\000\000\000\040\000\
\\000\000\048\000\021\000\024\000\
\\000\000\006\000\027\000\029\000\
\\032\000\023\000\045\000\000\000\
\\000\000\025\000\005\000\030\000\
\\000\000\028\000\014\000"
val gotoT =
"\
\\001\000\002\000\003\000\074\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\004\000\028\000\005\000\027\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\031\000\006\000\001\000\000\000\
\\001\000\032\000\006\000\001\000\000\000\
\\001\000\033\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\036\000\006\000\001\000\000\000\
\\001\000\037\000\006\000\001\000\000\000\
\\001\000\038\000\006\000\001\000\000\000\
\\001\000\039\000\006\000\001\000\000\000\
\\001\000\040\000\006\000\001\000\000\000\
\\001\000\041\000\006\000\001\000\000\000\
\\001\000\042\000\006\000\001\000\000\000\
\\001\000\043\000\006\000\001\000\000\000\
\\001\000\044\000\006\000\001\000\000\000\
\\001\000\045\000\006\000\001\000\000\000\
\\001\000\046\000\006\000\001\000\000\000\
\\001\000\047\000\006\000\001\000\000\000\
\\001\000\048\000\006\000\001\000\000\000\
\\001\000\049\000\006\000\001\000\000\000\
\\004\000\050\000\005\000\027\000\000\000\
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
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\059\000\002\000\058\000\006\000\001\000\000\000\
\\000\000\
\\001\000\061\000\006\000\001\000\000\000\
\\001\000\062\000\006\000\001\000\000\000\
\\000\000\
\\001\000\063\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\066\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\059\000\002\000\069\000\006\000\001\000\000\000\
\\000\000\
\\001\000\070\000\006\000\001\000\000\000\
\\001\000\071\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\073\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 75
val numrules = 33
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
datatype svalue = VOID | ntVOID of unit | IDENTIFIER of  (string) | CONST of  (int) | LVALUE of  (Ast.Expr) | DEC of  (Ast.Dec) | DECS of  (Ast.Dec list) | PROGRAM of  (Ast.Expr) | EXPS of  (Ast.Expr list) | EXP of  (Ast.Expr)
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
  | (T 22) => "DO"
  | (T 23) => "FOR"
  | (T 24) => "TO"
  | (T 25) => "BREAK"
  | (T 26) => "IDENTIFIER"
  | (T 27) => "SEMICOLON"
  | (T 28) => "LET"
  | (T 29) => "IN"
  | (T 30) => "END"
  | (T 31) => "VAR"
  | (T 32) => "UMINUS"
  | (T 33) => "LBRACK"
  | (T 34) => "RBRACK"
  | (T 35) => "DOT"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.EXP EXP, EXP1left, EXP1right)) :: rest671)) => let val  result = MlyValue.PROGRAM ((*#line 75.30 "tiger.grm"*) EXP (*#line 369.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, EXP1left, EXP1right), rest671)
end
|  ( 1, ( rest671)) => let val  result = MlyValue.EXPS ((*#line 77.33 "tiger.grm"*) []                  (*#line 373.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 2, ( ( _, ( MlyValue.EXP EXP, EXP1left, EXP1right)) :: rest671)) => let val  result = MlyValue.EXPS ((*#line 78.18 "tiger.grm"*) EXP :: []         (*#line 377.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, EXP1left, EXP1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.EXPS EXPS, _, EXPS1right)) :: _ :: ( _, ( MlyValue.EXP EXP, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXPS ((*#line 79.28 "tiger.grm"*) EXP :: EXPS         (*#line 381.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, EXP1left, EXPS1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 81.43 "tiger.grm"*)Ast.Assign (LVALUE, EXP)(*#line 385.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, LVALUE1left, EXP1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, WHILE1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 82.23 "tiger.grm"*)Ast.While EXP1 EXP2(*#line 389.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, WHILE1left, EXP2right), rest671)
end
|  ( 6, ( ( _, ( _, _, EXP3right)) :: _ :: ( _, ( MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP (EXP1 as EXP), _, _)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, _, _)) :: ( _, ( _, FOR1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 83.55 "tiger.grm"*)Ast.For IDENTIFIER EXP1 EXP2 EXP(*#line 393.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, FOR1left, EXP3right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 84.25 "tiger.grm"*)Ast.Openif EXP1 EXP2 (*#line 397.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, IF1left, EXP2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.EXP EXP3, _, EXP3right)) :: _ :: ( _, ( MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 85.30 "tiger.grm"*)Ast.Closedif  EXP1 EXP2 EXP3(*#line 401.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, IF1left, EXP3right), rest671)
end
|  ( 9, ( ( _, ( _, BREAK1left, BREAK1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 86.13 "tiger.grm"*)Ast.BREAK	(*#line 405.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 10, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.EXPS EXPS, _, _)) :: _ :: ( _, ( MlyValue.DECS DECS, _, _)) :: ( _, ( _, LET1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 87.27 "tiger.grm"*)Ast.LET (DECS, EXPS) (*#line 409.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, LET1left, END1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 88.25 "tiger.grm"*)Ast.equal  EXP1 EXP2(*#line 413.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 89.27 "tiger.grm"*)Ast.notequal  EXP1 EXP2(*#line 417.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 90.27 "tiger.grm"*)Ast.lessequal  EXP1 EXP2(*#line 421.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 91.25 "tiger.grm"*)Ast.less  EXP1 EXP2(*#line 425.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 92.27 "tiger.grm"*)Ast.greatequal  EXP1 EXP2(*#line 429.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 93.25 "tiger.grm"*)Ast.great  EXP1 EXP2(*#line 433.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 94.26 "tiger.grm"*)Ast.logicaland  EXP1 EXP2(*#line 437.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 95.25 "tiger.grm"*)Ast.or  EXP1 EXP2(*#line 441.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 96.26 "tiger.grm"*) Ast.mul   EXP1 EXP2 (*#line 445.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 97.30 "tiger.grm"*)Ast.divide  EXP1 EXP2(*#line 449.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 98.30 "tiger.grm"*) Ast.plus  EXP1 EXP2 (*#line 453.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 99.26 "tiger.grm"*) Ast.minus EXP1 EXP2 (*#line 457.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.CONST CONST, CONST1left, CONST1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 100.26 "tiger.grm"*) Ast.Const CONST     (*#line 461.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, CONST1left, CONST1right), rest671)
end
|  ( 24, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 101.25 "tiger.grm"*)EXP(*#line 465.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.CONST CONST, _, CONST1right)) :: ( _, ( _, MINUS1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 102.34 "tiger.grm"*)Ast.NegConst  CONST   (*#line 469.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, MINUS1left, CONST1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, LVALUE1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 103.13 "tiger.grm"*)LVALUE(*#line 473.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, LVALUE1left, LVALUE1right), rest671)
end
|  ( 27, ( rest671)) => let val  result = MlyValue.DECS ((*#line 105.23 "tiger.grm"*)[](*#line 477.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, defaultPos, defaultPos), rest671)
end
|  ( 28, ( ( _, ( MlyValue.DECS DECS, _, DECS1right)) :: ( _, ( MlyValue.DEC DEC, DEC1left, _)) :: rest671)) => let val  result = MlyValue.DECS ((*#line 106.14 "tiger.grm"*)DEC :: DECS	(*#line 481.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, DEC1left, DECS1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 108.34 "tiger.grm"*)Ast.VarDec(IDENTIFIER, EXP) (*#line 485.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, VAR1left, EXP1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER, IDENTIFIER1left, IDENTIFIER1right)) :: rest671)) => let val  result = MlyValue.LVALUE ((*#line 110.24 "tiger.grm"*)Ast.Name IDENTIFIER(*#line 489.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, IDENTIFIER1left, IDENTIFIER1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER, _, IDENTIFIER1right)) :: _ :: ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, _)) :: rest671)) => let val  result = MlyValue.LVALUE ((*#line 111.27 "tiger.grm"*)Ast.Method ( LVALUE, IDENTIFIER )(*#line 493.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, LVALUE1left, IDENTIFIER1right), rest671)
end
|  ( 32, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: _ :: ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, _)) :: rest671)) => let val  result = MlyValue.LVALUE ((*#line 112.29 "tiger.grm"*)Ast.Access( LVALUE, EXP)(*#line 497.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, LVALUE1left, RBRACK1right), rest671)
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
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(ParserData.MlyValue.VOID,p1,p2))
fun TO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(ParserData.MlyValue.VOID,p1,p2))
fun IDENTIFIER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(ParserData.MlyValue.IDENTIFIER i,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(ParserData.MlyValue.VOID,p1,p2))
fun UMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(ParserData.MlyValue.VOID,p1,p2))
end
end
