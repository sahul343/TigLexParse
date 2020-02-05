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
\\001\000\001\000\014\000\003\000\013\000\016\000\012\000\020\000\011\000\
\\022\000\010\000\024\000\009\000\026\000\008\000\027\000\007\000\
\\029\000\006\000\037\000\005\000\000\000\
\\001\000\001\000\038\000\000\000\
\\001\000\002\000\029\000\003\000\028\000\004\000\027\000\005\000\026\000\
\\006\000\025\000\007\000\024\000\008\000\023\000\009\000\022\000\
\\010\000\021\000\011\000\020\000\012\000\019\000\013\000\018\000\
\\018\000\061\000\000\000\
\\001\000\002\000\029\000\003\000\028\000\004\000\027\000\005\000\026\000\
\\006\000\025\000\007\000\024\000\008\000\023\000\009\000\022\000\
\\010\000\021\000\011\000\020\000\012\000\019\000\013\000\018\000\
\\021\000\060\000\000\000\
\\001\000\002\000\029\000\003\000\028\000\004\000\027\000\005\000\026\000\
\\006\000\025\000\007\000\024\000\008\000\023\000\009\000\022\000\
\\010\000\021\000\011\000\020\000\012\000\019\000\013\000\018\000\
\\023\000\059\000\000\000\
\\001\000\002\000\029\000\003\000\028\000\004\000\027\000\005\000\026\000\
\\006\000\025\000\007\000\024\000\008\000\023\000\009\000\022\000\
\\010\000\021\000\011\000\020\000\012\000\019\000\013\000\018\000\
\\023\000\080\000\000\000\
\\001\000\002\000\029\000\003\000\028\000\004\000\027\000\005\000\026\000\
\\006\000\025\000\007\000\024\000\008\000\023\000\009\000\022\000\
\\010\000\021\000\011\000\020\000\012\000\019\000\013\000\018\000\
\\025\000\074\000\000\000\
\\001\000\002\000\029\000\003\000\028\000\004\000\027\000\005\000\026\000\
\\006\000\025\000\007\000\024\000\008\000\023\000\009\000\022\000\
\\010\000\021\000\011\000\020\000\012\000\019\000\013\000\018\000\
\\035\000\062\000\000\000\
\\001\000\002\000\029\000\003\000\028\000\004\000\027\000\005\000\026\000\
\\006\000\025\000\007\000\024\000\008\000\023\000\009\000\022\000\
\\010\000\021\000\011\000\020\000\012\000\019\000\013\000\018\000\
\\035\000\066\000\000\000\
\\001\000\002\000\029\000\003\000\028\000\004\000\027\000\005\000\026\000\
\\011\000\094\000\012\000\094\000\015\000\094\000\017\000\094\000\
\\018\000\094\000\021\000\094\000\023\000\094\000\025\000\094\000\
\\028\000\094\000\030\000\094\000\031\000\094\000\032\000\094\000\
\\035\000\094\000\000\000\
\\001\000\002\000\029\000\003\000\028\000\004\000\027\000\005\000\026\000\
\\011\000\095\000\012\000\095\000\015\000\095\000\017\000\095\000\
\\018\000\095\000\021\000\095\000\023\000\095\000\025\000\095\000\
\\028\000\095\000\030\000\095\000\031\000\095\000\032\000\095\000\
\\035\000\095\000\000\000\
\\001\000\002\000\029\000\003\000\028\000\004\000\027\000\005\000\026\000\
\\011\000\096\000\012\000\096\000\015\000\096\000\017\000\096\000\
\\018\000\096\000\021\000\096\000\023\000\096\000\025\000\096\000\
\\028\000\096\000\030\000\096\000\031\000\096\000\032\000\096\000\
\\035\000\096\000\000\000\
\\001\000\002\000\029\000\003\000\028\000\004\000\027\000\005\000\026\000\
\\011\000\097\000\012\000\097\000\015\000\097\000\017\000\097\000\
\\018\000\097\000\021\000\097\000\023\000\097\000\025\000\097\000\
\\028\000\097\000\030\000\097\000\031\000\097\000\032\000\097\000\
\\035\000\097\000\000\000\
\\001\000\002\000\029\000\003\000\028\000\004\000\027\000\005\000\026\000\
\\011\000\098\000\012\000\098\000\015\000\098\000\017\000\098\000\
\\018\000\098\000\021\000\098\000\023\000\098\000\025\000\098\000\
\\028\000\098\000\030\000\098\000\031\000\098\000\032\000\098\000\
\\035\000\098\000\000\000\
\\001\000\002\000\029\000\003\000\028\000\004\000\027\000\005\000\026\000\
\\011\000\099\000\012\000\099\000\015\000\099\000\017\000\099\000\
\\018\000\099\000\021\000\099\000\023\000\099\000\025\000\099\000\
\\028\000\099\000\030\000\099\000\031\000\099\000\032\000\099\000\
\\035\000\099\000\000\000\
\\001\000\015\000\000\000\000\000\
\\001\000\019\000\058\000\000\000\
\\001\000\019\000\065\000\000\000\
\\001\000\027\000\034\000\000\000\
\\001\000\027\000\039\000\000\000\
\\001\000\027\000\056\000\000\000\
\\001\000\030\000\055\000\000\000\
\\001\000\031\000\070\000\000\000\
\\001\000\038\000\073\000\000\000\
\\083\000\002\000\029\000\003\000\028\000\004\000\027\000\005\000\026\000\
\\006\000\025\000\007\000\024\000\008\000\023\000\009\000\022\000\
\\010\000\021\000\011\000\020\000\012\000\019\000\013\000\018\000\000\000\
\\084\000\001\000\014\000\003\000\013\000\016\000\012\000\020\000\011\000\
\\022\000\010\000\024\000\009\000\026\000\008\000\027\000\007\000\
\\029\000\006\000\037\000\005\000\000\000\
\\085\000\002\000\029\000\003\000\028\000\004\000\027\000\005\000\026\000\
\\006\000\025\000\007\000\024\000\008\000\023\000\009\000\022\000\
\\010\000\021\000\011\000\020\000\012\000\019\000\013\000\018\000\
\\028\000\071\000\000\000\
\\086\000\000\000\
\\087\000\002\000\029\000\003\000\028\000\004\000\027\000\005\000\026\000\
\\006\000\025\000\007\000\024\000\008\000\023\000\009\000\022\000\
\\010\000\021\000\011\000\020\000\012\000\019\000\013\000\018\000\000\000\
\\088\000\002\000\029\000\003\000\028\000\004\000\027\000\005\000\026\000\
\\006\000\025\000\007\000\024\000\008\000\023\000\009\000\022\000\
\\010\000\021\000\011\000\020\000\012\000\019\000\013\000\018\000\000\000\
\\089\000\002\000\029\000\003\000\028\000\004\000\027\000\005\000\026\000\
\\006\000\025\000\007\000\024\000\008\000\023\000\009\000\022\000\
\\010\000\021\000\011\000\020\000\012\000\019\000\013\000\018\000\000\000\
\\090\000\002\000\029\000\003\000\028\000\004\000\027\000\005\000\026\000\
\\006\000\025\000\007\000\024\000\008\000\023\000\009\000\022\000\
\\010\000\021\000\011\000\020\000\012\000\019\000\013\000\018\000\
\\017\000\075\000\000\000\
\\091\000\002\000\029\000\003\000\028\000\004\000\027\000\005\000\026\000\
\\006\000\025\000\007\000\024\000\008\000\023\000\009\000\022\000\
\\010\000\021\000\011\000\020\000\012\000\019\000\013\000\018\000\000\000\
\\092\000\000\000\
\\093\000\000\000\
\\100\000\002\000\029\000\003\000\028\000\004\000\027\000\005\000\026\000\
\\006\000\025\000\007\000\024\000\008\000\023\000\009\000\022\000\
\\010\000\021\000\013\000\018\000\000\000\
\\101\000\002\000\029\000\003\000\028\000\004\000\027\000\005\000\026\000\
\\006\000\025\000\007\000\024\000\008\000\023\000\009\000\022\000\
\\010\000\021\000\011\000\020\000\013\000\018\000\000\000\
\\102\000\000\000\
\\103\000\000\000\
\\104\000\004\000\027\000\005\000\026\000\000\000\
\\105\000\004\000\027\000\005\000\026\000\000\000\
\\106\000\000\000\
\\107\000\000\000\
\\108\000\000\000\
\\109\000\019\000\017\000\034\000\016\000\036\000\015\000\000\000\
\\110\000\000\000\
\\111\000\002\000\029\000\003\000\028\000\004\000\027\000\005\000\026\000\
\\006\000\025\000\007\000\024\000\008\000\023\000\009\000\022\000\
\\010\000\021\000\011\000\020\000\012\000\019\000\013\000\018\000\000\000\
\\112\000\032\000\032\000\000\000\
\\113\000\000\000\
\\114\000\002\000\029\000\003\000\028\000\004\000\027\000\005\000\026\000\
\\006\000\025\000\007\000\024\000\008\000\023\000\009\000\022\000\
\\010\000\021\000\011\000\020\000\012\000\019\000\013\000\018\000\000\000\
\\115\000\034\000\033\000\000\000\
\\116\000\000\000\
\\117\000\000\000\
\"
val actionRowNumbers =
"\000\000\044\000\024\000\045\000\
\\047\000\050\000\033\000\018\000\
\\000\000\000\000\000\000\001\000\
\\041\000\019\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\047\000\021\000\020\000\000\000\
\\016\000\004\000\003\000\002\000\
\\043\000\051\000\007\000\028\000\
\\010\000\036\000\035\000\014\000\
\\013\000\011\000\009\000\012\000\
\\038\000\037\000\040\000\039\000\
\\048\000\025\000\017\000\008\000\
\\000\000\000\000\042\000\000\000\
\\052\000\022\000\026\000\000\000\
\\023\000\006\000\029\000\031\000\
\\034\000\025\000\049\000\000\000\
\\000\000\000\000\027\000\046\000\
\\005\000\032\000\000\000\030\000\
\\015\000"
val gotoT =
"\
\\001\000\002\000\003\000\080\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\029\000\005\000\028\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\033\000\006\000\001\000\000\000\
\\001\000\034\000\006\000\001\000\000\000\
\\001\000\035\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
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
\\001\000\050\000\006\000\001\000\000\000\
\\001\000\051\000\006\000\001\000\000\000\
\\004\000\052\000\005\000\028\000\000\000\
\\000\000\
\\000\000\
\\001\000\055\000\006\000\001\000\000\000\
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
\\001\000\062\000\002\000\061\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\001\000\065\000\006\000\001\000\000\000\
\\001\000\066\000\006\000\001\000\000\000\
\\000\000\
\\001\000\067\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\070\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\062\000\002\000\074\000\006\000\001\000\000\000\
\\000\000\
\\001\000\075\000\006\000\001\000\000\000\
\\001\000\076\000\006\000\001\000\000\000\
\\001\000\077\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\079\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 81
val numrules = 35
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
  | (T 36) => "NIL"
  | (T 37) => "OF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.EXP EXP, EXP1left, EXP1right)) :: rest671)) => let val  result = MlyValue.PROGRAM ((*#line 80.30 "tiger.grm"*) EXP (*#line 388.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, EXP1left, EXP1right), rest671)
end
|  ( 1, ( rest671)) => let val  result = MlyValue.EXPS ((*#line 82.33 "tiger.grm"*) []                  (*#line 392.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 2, ( ( _, ( MlyValue.EXP EXP, EXP1left, EXP1right)) :: rest671)) => let val  result = MlyValue.EXPS ((*#line 83.18 "tiger.grm"*) EXP :: []         (*#line 396.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, EXP1left, EXP1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.EXPS EXPS, _, EXPS1right)) :: _ :: ( _, ( MlyValue.EXP EXP, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXPS ((*#line 84.28 "tiger.grm"*) EXP :: EXPS         (*#line 400.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, EXP1left, EXPS1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 86.43 "tiger.grm"*)Ast.Assign (LVALUE, EXP)(*#line 404.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, LVALUE1left, EXP1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, WHILE1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 87.23 "tiger.grm"*)Ast.While EXP1 EXP2(*#line 408.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, WHILE1left, EXP2right), rest671)
end
|  ( 6, ( ( _, ( _, _, EXP3right)) :: _ :: ( _, ( MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP (EXP1 as EXP), _, _)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, _, _)) :: ( _, ( _, FOR1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 88.55 "tiger.grm"*)Ast.For IDENTIFIER EXP1 EXP2 EXP(*#line 412.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, FOR1left, EXP3right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 89.25 "tiger.grm"*)Ast.Openif EXP1 EXP2 (*#line 416.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, IF1left, EXP2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.EXP EXP3, _, EXP3right)) :: _ :: ( _, ( MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 90.30 "tiger.grm"*)Ast.Closedif  EXP1 EXP2 EXP3(*#line 420.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, IF1left, EXP3right), rest671)
end
|  ( 9, ( ( _, ( _, BREAK1left, BREAK1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 91.13 "tiger.grm"*)Ast.BREAK	(*#line 424.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 10, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.EXPS EXPS, _, _)) :: _ :: ( _, ( MlyValue.DECS DECS, _, _)) :: ( _, ( _, LET1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 92.27 "tiger.grm"*)Ast.LET (DECS, EXPS) (*#line 428.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, LET1left, END1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 93.25 "tiger.grm"*)Ast.equal  EXP1 EXP2(*#line 432.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 94.27 "tiger.grm"*)Ast.notequal  EXP1 EXP2(*#line 436.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 95.27 "tiger.grm"*)Ast.lessequal  EXP1 EXP2(*#line 440.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 96.25 "tiger.grm"*)Ast.less  EXP1 EXP2(*#line 444.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 97.27 "tiger.grm"*)Ast.greatequal  EXP1 EXP2(*#line 448.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 98.25 "tiger.grm"*)Ast.great  EXP1 EXP2(*#line 452.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 99.26 "tiger.grm"*)Ast.logicaland  EXP1 EXP2(*#line 456.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 100.25 "tiger.grm"*)Ast.or  EXP1 EXP2(*#line 460.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 101.26 "tiger.grm"*) Ast.mul   EXP1 EXP2 (*#line 464.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 102.30 "tiger.grm"*)Ast.divide  EXP1 EXP2(*#line 468.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 103.30 "tiger.grm"*) Ast.plus  EXP1 EXP2 (*#line 472.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 104.26 "tiger.grm"*) Ast.minus EXP1 EXP2 (*#line 476.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.CONST CONST, CONST1left, CONST1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 105.26 "tiger.grm"*) Ast.Const CONST     (*#line 480.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, CONST1left, CONST1right), rest671)
end
|  ( 24, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 106.25 "tiger.grm"*)EXP(*#line 484.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.CONST CONST, _, CONST1right)) :: ( _, ( _, MINUS1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 107.34 "tiger.grm"*)Ast.NegConst  CONST   (*#line 488.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, MINUS1left, CONST1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, LVALUE1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 108.13 "tiger.grm"*)LVALUE(*#line 492.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, LVALUE1left, LVALUE1right), rest671)
end
|  ( 27, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 109.12 "tiger.grm"*)Ast.NIL(*#line 496.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, NIL1left, NIL1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, IDENTIFIER1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 110.49 "tiger.grm"*)Ast.Array(IDENTIFIER,EXP1,EXP2) (*#line 500.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, IDENTIFIER1left, EXP2right), rest671)
end
|  ( 29, ( rest671)) => let val  result = MlyValue.DECS ((*#line 112.23 "tiger.grm"*)[](*#line 504.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, defaultPos, defaultPos), rest671)
end
|  ( 30, ( ( _, ( MlyValue.DECS DECS, _, DECS1right)) :: ( _, ( MlyValue.DEC DEC, DEC1left, _)) :: rest671)) => let val  result = MlyValue.DECS ((*#line 113.14 "tiger.grm"*)DEC :: DECS	(*#line 508.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, DEC1left, DECS1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 115.34 "tiger.grm"*)Ast.VarDec(IDENTIFIER, EXP) (*#line 512.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, VAR1left, EXP1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER, IDENTIFIER1left, IDENTIFIER1right)) :: rest671)) => let val  result = MlyValue.LVALUE ((*#line 117.24 "tiger.grm"*)Ast.Name IDENTIFIER(*#line 516.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, IDENTIFIER1left, IDENTIFIER1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER, _, IDENTIFIER1right)) :: _ :: ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, _)) :: rest671)) => let val  result = MlyValue.LVALUE ((*#line 118.27 "tiger.grm"*)Ast.Method ( LVALUE, IDENTIFIER )(*#line 520.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, LVALUE1left, IDENTIFIER1right), rest671)
end
|  ( 34, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: _ :: ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, _)) :: rest671)) => let val  result = MlyValue.LVALUE ((*#line 119.29 "tiger.grm"*)Ast.Access( LVALUE, EXP)(*#line 524.1 "tiger.grm.sml"*)
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
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(ParserData.MlyValue.VOID,p1,p2))
end
end
