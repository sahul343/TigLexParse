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
\\001\000\001\000\015\000\003\000\014\000\016\000\013\000\020\000\012\000\
\\022\000\011\000\024\000\010\000\026\000\009\000\027\000\008\000\
\\029\000\007\000\037\000\006\000\042\000\005\000\000\000\
\\001\000\001\000\042\000\000\000\
\\001\000\002\000\030\000\003\000\029\000\004\000\028\000\005\000\027\000\
\\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\011\000\021\000\012\000\020\000\013\000\019\000\
\\018\000\069\000\000\000\
\\001\000\002\000\030\000\003\000\029\000\004\000\028\000\005\000\027\000\
\\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\011\000\021\000\012\000\020\000\013\000\019\000\
\\021\000\068\000\000\000\
\\001\000\002\000\030\000\003\000\029\000\004\000\028\000\005\000\027\000\
\\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\011\000\021\000\012\000\020\000\013\000\019\000\
\\023\000\067\000\000\000\
\\001\000\002\000\030\000\003\000\029\000\004\000\028\000\005\000\027\000\
\\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\011\000\021\000\012\000\020\000\013\000\019\000\
\\023\000\104\000\000\000\
\\001\000\002\000\030\000\003\000\029\000\004\000\028\000\005\000\027\000\
\\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\011\000\021\000\012\000\020\000\013\000\019\000\
\\025\000\092\000\000\000\
\\001\000\002\000\030\000\003\000\029\000\004\000\028\000\005\000\027\000\
\\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\011\000\021\000\012\000\020\000\013\000\019\000\
\\035\000\071\000\000\000\
\\001\000\002\000\030\000\003\000\029\000\004\000\028\000\005\000\027\000\
\\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\011\000\021\000\012\000\020\000\013\000\019\000\
\\035\000\077\000\000\000\
\\001\000\002\000\030\000\003\000\029\000\004\000\028\000\005\000\027\000\
\\011\000\122\000\012\000\122\000\015\000\122\000\017\000\122\000\
\\018\000\122\000\021\000\122\000\023\000\122\000\025\000\122\000\
\\028\000\122\000\030\000\122\000\031\000\122\000\032\000\122\000\
\\035\000\122\000\039\000\122\000\041\000\122\000\000\000\
\\001\000\002\000\030\000\003\000\029\000\004\000\028\000\005\000\027\000\
\\011\000\123\000\012\000\123\000\015\000\123\000\017\000\123\000\
\\018\000\123\000\021\000\123\000\023\000\123\000\025\000\123\000\
\\028\000\123\000\030\000\123\000\031\000\123\000\032\000\123\000\
\\035\000\123\000\039\000\123\000\041\000\123\000\000\000\
\\001\000\002\000\030\000\003\000\029\000\004\000\028\000\005\000\027\000\
\\011\000\124\000\012\000\124\000\015\000\124\000\017\000\124\000\
\\018\000\124\000\021\000\124\000\023\000\124\000\025\000\124\000\
\\028\000\124\000\030\000\124\000\031\000\124\000\032\000\124\000\
\\035\000\124\000\039\000\124\000\041\000\124\000\000\000\
\\001\000\002\000\030\000\003\000\029\000\004\000\028\000\005\000\027\000\
\\011\000\125\000\012\000\125\000\015\000\125\000\017\000\125\000\
\\018\000\125\000\021\000\125\000\023\000\125\000\025\000\125\000\
\\028\000\125\000\030\000\125\000\031\000\125\000\032\000\125\000\
\\035\000\125\000\039\000\125\000\041\000\125\000\000\000\
\\001\000\002\000\030\000\003\000\029\000\004\000\028\000\005\000\027\000\
\\011\000\126\000\012\000\126\000\015\000\126\000\017\000\126\000\
\\018\000\126\000\021\000\126\000\023\000\126\000\025\000\126\000\
\\028\000\126\000\030\000\126\000\031\000\126\000\032\000\126\000\
\\035\000\126\000\039\000\126\000\041\000\126\000\000\000\
\\001\000\002\000\030\000\003\000\029\000\004\000\028\000\005\000\027\000\
\\011\000\127\000\012\000\127\000\015\000\127\000\017\000\127\000\
\\018\000\127\000\021\000\127\000\023\000\127\000\025\000\127\000\
\\028\000\127\000\030\000\127\000\031\000\127\000\032\000\127\000\
\\035\000\127\000\039\000\127\000\041\000\127\000\000\000\
\\001\000\007\000\076\000\000\000\
\\001\000\007\000\105\000\000\000\
\\001\000\015\000\000\000\000\000\
\\001\000\019\000\066\000\000\000\
\\001\000\019\000\074\000\000\000\
\\001\000\021\000\078\000\000\000\
\\001\000\021\000\094\000\000\000\
\\001\000\027\000\031\000\000\000\
\\001\000\027\000\038\000\000\000\
\\001\000\027\000\043\000\000\000\
\\001\000\027\000\060\000\000\000\
\\001\000\027\000\103\000\000\000\
\\001\000\030\000\059\000\000\000\
\\001\000\031\000\085\000\000\000\
\\001\000\041\000\075\000\000\000\
\\110\000\002\000\030\000\003\000\029\000\004\000\028\000\005\000\027\000\
\\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\011\000\021\000\012\000\020\000\013\000\019\000\000\000\
\\111\000\001\000\015\000\003\000\014\000\016\000\013\000\020\000\012\000\
\\022\000\011\000\024\000\010\000\026\000\009\000\027\000\008\000\
\\029\000\007\000\037\000\006\000\042\000\005\000\000\000\
\\112\000\000\000\
\\113\000\002\000\030\000\003\000\029\000\004\000\028\000\005\000\027\000\
\\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\011\000\021\000\012\000\020\000\013\000\019\000\
\\028\000\087\000\000\000\
\\114\000\000\000\
\\115\000\002\000\030\000\003\000\029\000\004\000\028\000\005\000\027\000\
\\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\011\000\021\000\012\000\020\000\013\000\019\000\000\000\
\\116\000\002\000\030\000\003\000\029\000\004\000\028\000\005\000\027\000\
\\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\011\000\021\000\012\000\020\000\013\000\019\000\000\000\
\\117\000\002\000\030\000\003\000\029\000\004\000\028\000\005\000\027\000\
\\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\011\000\021\000\012\000\020\000\013\000\019\000\000\000\
\\118\000\002\000\030\000\003\000\029\000\004\000\028\000\005\000\027\000\
\\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\011\000\021\000\012\000\020\000\013\000\019\000\
\\017\000\093\000\000\000\
\\119\000\002\000\030\000\003\000\029\000\004\000\028\000\005\000\027\000\
\\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\011\000\021\000\012\000\020\000\013\000\019\000\000\000\
\\120\000\000\000\
\\121\000\000\000\
\\128\000\002\000\030\000\003\000\029\000\004\000\028\000\005\000\027\000\
\\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\013\000\019\000\000\000\
\\129\000\002\000\030\000\003\000\029\000\004\000\028\000\005\000\027\000\
\\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\011\000\021\000\013\000\019\000\000\000\
\\130\000\000\000\
\\131\000\000\000\
\\132\000\004\000\028\000\005\000\027\000\000\000\
\\133\000\004\000\028\000\005\000\027\000\000\000\
\\134\000\000\000\
\\135\000\000\000\
\\136\000\000\000\
\\137\000\019\000\018\000\034\000\017\000\036\000\016\000\000\000\
\\138\000\000\000\
\\139\000\002\000\030\000\003\000\029\000\004\000\028\000\005\000\027\000\
\\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\011\000\021\000\012\000\020\000\013\000\019\000\000\000\
\\140\000\000\000\
\\141\000\000\000\
\\142\000\000\000\
\\143\000\000\000\
\\144\000\001\000\015\000\003\000\014\000\016\000\013\000\020\000\012\000\
\\022\000\011\000\024\000\010\000\026\000\009\000\027\000\008\000\
\\029\000\007\000\037\000\006\000\042\000\005\000\000\000\
\\145\000\000\000\
\\146\000\002\000\030\000\003\000\029\000\004\000\028\000\005\000\027\000\
\\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\011\000\021\000\012\000\020\000\013\000\019\000\
\\039\000\080\000\000\000\
\\147\000\000\000\
\\148\000\027\000\062\000\000\000\
\\149\000\000\000\
\\150\000\002\000\030\000\003\000\029\000\004\000\028\000\005\000\027\000\
\\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\011\000\021\000\012\000\020\000\013\000\019\000\
\\039\000\097\000\000\000\
\\151\000\000\000\
\\152\000\032\000\034\000\000\000\
\\153\000\000\000\
\\154\000\002\000\030\000\003\000\029\000\004\000\028\000\005\000\027\000\
\\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\011\000\021\000\012\000\020\000\013\000\019\000\000\000\
\\155\000\020\000\037\000\034\000\036\000\040\000\035\000\000\000\
\\156\000\020\000\070\000\000\000\
\\157\000\000\000\
\\158\000\038\000\090\000\000\000\
\"
val actionRowNumbers =
"\000\000\051\000\030\000\022\000\
\\052\000\066\000\069\000\040\000\
\\023\000\000\000\000\000\000\000\
\\001\000\048\000\024\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\055\000\066\000\027\000\
\\025\000\062\000\000\000\058\000\
\\018\000\004\000\003\000\002\000\
\\050\000\070\000\007\000\035\000\
\\010\000\043\000\042\000\014\000\
\\013\000\011\000\009\000\012\000\
\\045\000\044\000\047\000\046\000\
\\067\000\031\000\019\000\029\000\
\\015\000\008\000\020\000\060\000\
\\000\000\000\000\049\000\000\000\
\\058\000\071\000\028\000\033\000\
\\000\000\054\000\000\000\072\000\
\\056\000\059\000\000\000\006\000\
\\036\000\038\000\021\000\041\000\
\\032\000\000\000\068\000\064\000\
\\000\000\060\000\000\000\000\000\
\\057\000\033\000\063\000\026\000\
\\053\000\061\000\005\000\039\000\
\\034\000\016\000\000\000\000\000\
\\037\000\064\000\065\000\017\000"
val gotoT =
"\
\\001\000\002\000\003\000\107\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\031\000\005\000\030\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\037\000\006\000\001\000\000\000\
\\001\000\038\000\006\000\001\000\000\000\
\\001\000\039\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
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
\\001\000\052\000\006\000\001\000\000\000\
\\001\000\053\000\006\000\001\000\000\000\
\\001\000\054\000\006\000\001\000\000\000\
\\001\000\055\000\006\000\001\000\000\000\
\\000\000\
\\004\000\056\000\005\000\030\000\000\000\
\\000\000\
\\000\000\
\\008\000\059\000\000\000\
\\001\000\061\000\006\000\001\000\000\000\
\\001\000\063\000\006\000\001\000\010\000\062\000\000\000\
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
\\001\000\071\000\002\000\070\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\077\000\000\000\
\\001\000\079\000\006\000\001\000\000\000\
\\001\000\080\000\006\000\001\000\000\000\
\\000\000\
\\001\000\081\000\006\000\001\000\000\000\
\\001\000\063\000\006\000\001\000\010\000\082\000\000\000\
\\000\000\
\\000\000\
\\012\000\084\000\000\000\
\\001\000\086\000\006\000\001\000\000\000\
\\000\000\
\\001\000\087\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\089\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\093\000\006\000\001\000\000\000\
\\000\000\
\\009\000\094\000\000\000\
\\001\000\096\000\006\000\001\000\000\000\
\\011\000\097\000\000\000\
\\001\000\098\000\006\000\001\000\000\000\
\\001\000\099\000\006\000\001\000\000\000\
\\000\000\
\\012\000\100\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\104\000\006\000\001\000\000\000\
\\001\000\105\000\006\000\001\000\000\000\
\\000\000\
\\009\000\106\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 108
val numrules = 49
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
datatype svalue = VOID | ntVOID of unit | IDENTIFIER of  (string) | CONST of  (int) | EXPTAIL of  (Ast.Expr list) | ARGTAIL of  (Ast.Expr list) | ARGUMENTS of  (Ast.Expr list) | RECBODYTAIL of  ( ( string * Ast.Expr )  list) | RECBODY of  ( ( string * Ast.Expr )  list) | Record of  (Ast.Expr) | LVALUE of  (Ast.Expr) | DEC of  (Ast.Dec) | DECS of  (Ast.Dec list) | PROGRAM of  (Ast.Expr) | EXPS of  (Ast.Expr list) | EXP of  (Ast.Expr)
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
  | (T 38) => "COMMA"
  | (T 39) => "LBRACE"
  | (T 40) => "RBRACE"
  | (T 41) => "NEW"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.EXP EXP, EXP1left, EXP1right)) :: rest671)) => let val  result = MlyValue.PROGRAM ((*#line 89.30 "tiger.grm"*) EXP (*#line 453.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, EXP1left, EXP1right), rest671)
end
|  ( 1, ( rest671)) => let val  result = MlyValue.EXPS ((*#line 91.33 "tiger.grm"*) []                  (*#line 457.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 2, ( ( _, ( MlyValue.EXPTAIL EXPTAIL, _, EXPTAIL1right)) :: ( _, ( MlyValue.EXP EXP, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXPS ((*#line 92.18 "tiger.grm"*)EXP :: EXPTAIL	   (*#line 461.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, EXP1left, EXPTAIL1right), rest671)
end
|  ( 3, ( rest671)) => let val  result = MlyValue.EXPTAIL ((*#line 93.22 "tiger.grm"*)[](*#line 465.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 11, ( result, defaultPos, defaultPos), rest671)
end
|  ( 4, ( ( _, ( MlyValue.EXPTAIL EXPTAIL, _, EXPTAIL1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: ( _, ( _, SEMICOLON1left, _)) :: rest671)) => let val  result = MlyValue.EXPTAIL ((*#line 94.33 "tiger.grm"*)EXP::EXPTAIL(*#line 469.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 11, ( result, SEMICOLON1left, EXPTAIL1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 96.43 "tiger.grm"*)Ast.Assign (LVALUE, EXP)(*#line 473.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, LVALUE1left, EXP1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, WHILE1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 97.23 "tiger.grm"*)Ast.While EXP1 EXP2(*#line 477.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, WHILE1left, EXP2right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.EXP EXP3, _, EXP3right)) :: _ :: ( _, ( MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, _, _)) :: ( _, ( _, FOR1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 98.55 "tiger.grm"*)Ast.For IDENTIFIER EXP1 EXP2 EXP3(*#line 481.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, FOR1left, EXP3right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 99.25 "tiger.grm"*)Ast.Openif EXP1 EXP2 (*#line 485.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, IF1left, EXP2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.EXP EXP3, _, EXP3right)) :: _ :: ( _, ( MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 100.30 "tiger.grm"*)Ast.Closedif  EXP1 EXP2 EXP3(*#line 489.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, IF1left, EXP3right), rest671)
end
|  ( 10, ( ( _, ( _, BREAK1left, BREAK1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 101.13 "tiger.grm"*)Ast.BREAK	(*#line 493.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 11, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.EXPS EXPS, _, _)) :: _ :: ( _, ( MlyValue.DECS DECS, _, _)) :: ( _, ( _, LET1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 102.27 "tiger.grm"*)Ast.LET (DECS, EXPS) (*#line 497.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, LET1left, END1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 103.25 "tiger.grm"*)Ast.equal  EXP1 EXP2(*#line 501.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 104.27 "tiger.grm"*)Ast.notequal  EXP1 EXP2(*#line 505.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 105.27 "tiger.grm"*)Ast.lessequal  EXP1 EXP2(*#line 509.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 106.25 "tiger.grm"*)Ast.less  EXP1 EXP2(*#line 513.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 107.27 "tiger.grm"*)Ast.greatequal  EXP1 EXP2(*#line 517.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 108.25 "tiger.grm"*)Ast.great  EXP1 EXP2(*#line 521.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 109.26 "tiger.grm"*)Ast.logicaland  EXP1 EXP2(*#line 525.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 110.25 "tiger.grm"*)Ast.or  EXP1 EXP2(*#line 529.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 111.26 "tiger.grm"*) Ast.mul   EXP1 EXP2 (*#line 533.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 112.30 "tiger.grm"*)Ast.divide  EXP1 EXP2(*#line 537.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 113.30 "tiger.grm"*) Ast.plus  EXP1 EXP2 (*#line 541.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 114.26 "tiger.grm"*) Ast.minus EXP1 EXP2 (*#line 545.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.CONST CONST, CONST1left, CONST1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 115.26 "tiger.grm"*) Ast.Const CONST     (*#line 549.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, CONST1left, CONST1right), rest671)
end
|  ( 25, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 116.25 "tiger.grm"*)EXP(*#line 553.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.CONST CONST, _, CONST1right)) :: ( _, ( _, MINUS1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 117.34 "tiger.grm"*)Ast.NegConst  CONST   (*#line 557.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, MINUS1left, CONST1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, LVALUE1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 118.13 "tiger.grm"*)LVALUE(*#line 561.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, LVALUE1left, LVALUE1right), rest671)
end
|  ( 28, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 119.12 "tiger.grm"*)Ast.NIL(*#line 565.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, NIL1left, NIL1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, IDENTIFIER1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 120.49 "tiger.grm"*)Ast.Array(IDENTIFIER,EXP1,EXP2) (*#line 569.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, IDENTIFIER1left, EXP2right), rest671)
end
|  ( 30, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.RECBODY RECBODY, _, _)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, IDENTIFIER1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 121.39 "tiger.grm"*)Ast.Record (IDENTIFIER, RECBODY)(*#line 573.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, IDENTIFIER1left, RBRACE1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER, _, IDENTIFIER1right)) :: ( _, ( _, NEW1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 122.22 "tiger.grm"*)Ast.Object IDENTIFIER(*#line 577.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, NEW1left, IDENTIFIER1right), rest671)
end
|  ( 32, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ARGUMENTS ARGUMENTS, _, _)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, IDENTIFIER1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 123.41 "tiger.grm"*)Ast.FunCall (IDENTIFIER,ARGUMENTS) (*#line 581.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, IDENTIFIER1left, RPAREN1right), rest671)
end
|  ( 33, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ARGUMENTS ARGUMENTS, _, _)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, _, _)) :: _ :: ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 124.54 "tiger.grm"*)Ast.MethodCall (LVALUE,IDENTIFIER,ARGUMENTS) (*#line 585.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, LVALUE1left, RPAREN1right), rest671)
end
|  ( 34, ( rest671)) => let val  result = MlyValue.ARGUMENTS ((*#line 125.25 "tiger.grm"*)[](*#line 589.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 9, ( result, defaultPos, defaultPos), rest671)
end
|  ( 35, ( ( _, ( MlyValue.ARGTAIL ARGTAIL, _, ARGTAIL1right)) :: ( _, ( MlyValue.EXP EXP, EXP1left, _)) :: rest671)) => let val  result = MlyValue.ARGUMENTS ((*#line 126.18 "tiger.grm"*)EXP::ARGTAIL(*#line 593.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 9, ( result, EXP1left, ARGTAIL1right), rest671)
end
|  ( 36, ( rest671)) => let val  result = MlyValue.ARGTAIL ((*#line 127.15 "tiger.grm"*)[](*#line 597.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 10, ( result, defaultPos, defaultPos), rest671)
end
|  ( 37, ( ( _, ( MlyValue.ARGTAIL ARGTAIL, _, ARGTAIL1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: ( _, ( _, COMMA1left, _)) :: rest671)) => let val  result = MlyValue.ARGTAIL ((*#line 128.23 "tiger.grm"*)EXP::ARGTAIL(*#line 601.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 10, ( result, COMMA1left, ARGTAIL1right), rest671)
end
|  ( 38, ( rest671)) => let val  result = MlyValue.RECBODY ((*#line 129.24 "tiger.grm"*)[](*#line 605.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, defaultPos, defaultPos), rest671)
end
|  ( 39, ( ( _, ( MlyValue.RECBODYTAIL RECBODYTAIL, _, RECBODYTAIL1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, IDENTIFIER1left, _)) :: rest671)) => let val  result = MlyValue.RECBODY ((*#line 130.35 "tiger.grm"*)(IDENTIFIER, EXP)::RECBODYTAIL(*#line 609.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, IDENTIFIER1left, RECBODYTAIL1right), rest671)
end
|  ( 40, ( rest671)) => let val  result = MlyValue.RECBODYTAIL ((*#line 131.27 "tiger.grm"*)[](*#line 613.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, defaultPos, defaultPos), rest671)
end
|  ( 41, ( ( _, ( _, _, RECBODYTAIL1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, _, _)) :: ( _, ( _, COMMA1left, _)) :: rest671)) => let val  result = MlyValue.RECBODYTAIL ((*#line 132.40 "tiger.grm"*)(IDENTIFIER, EXP)::[](*#line 617.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, COMMA1left, RECBODYTAIL1right), rest671)
end
|  ( 42, ( rest671)) => let val  result = MlyValue.DECS ((*#line 133.23 "tiger.grm"*)[](*#line 621.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, defaultPos, defaultPos), rest671)
end
|  ( 43, ( ( _, ( MlyValue.DECS DECS, _, DECS1right)) :: ( _, ( MlyValue.DEC DEC, DEC1left, _)) :: rest671)) => let val  result = MlyValue.DECS ((*#line 134.14 "tiger.grm"*)DEC :: DECS	(*#line 625.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, DEC1left, DECS1right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 136.34 "tiger.grm"*)Ast.VarDec(IDENTIFIER, EXP) (*#line 629.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, VAR1left, EXP1right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER, IDENTIFIER1left, IDENTIFIER1right)) :: rest671)) => let val  result = MlyValue.LVALUE ((*#line 138.24 "tiger.grm"*)Ast.Name IDENTIFIER(*#line 633.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, IDENTIFIER1left, IDENTIFIER1right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER, _, IDENTIFIER1right)) :: _ :: ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, _)) :: rest671)) => let val  result = MlyValue.LVALUE ((*#line 139.27 "tiger.grm"*)Ast.Method ( LVALUE, IDENTIFIER )(*#line 637.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, LVALUE1left, IDENTIFIER1right), rest671)
end
|  ( 47, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: _ :: ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, _)) :: rest671)) => let val  result = MlyValue.LVALUE ((*#line 140.29 "tiger.grm"*)Ast.Access( LVALUE, EXP)(*#line 641.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, LVALUE1left, RBRACK1right), rest671)
end
|  ( 48, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER, IDENTIFIER1left, _)) :: rest671)) => let val  result = MlyValue.LVALUE ((*#line 141.33 "tiger.grm"*)Ast.Access( (Ast.Name IDENTIFIER), EXP)(*#line 645.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, IDENTIFIER1left, RBRACK1right), rest671)
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
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(ParserData.MlyValue.VOID,p1,p2))
fun NEW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(ParserData.MlyValue.VOID,p1,p2))
end
end
