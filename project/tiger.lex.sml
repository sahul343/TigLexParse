(*#line 55.10 "tiger.lex"*)functor TigerLexFun(structure Tokens : Tiger_TOKENS)(*#line 1.1 "tiger.lex.sml"*)
=
   struct
    structure UserDeclarations =
      struct
(*#line 1.1 "tiger.lex"*)(* Internal datatypes and functions required by the lexer *)
(* Keeping track of position in source                    *)

type lineNo            = int
type pos               = lineNo  (* The type of Should match with Tiger.yacc *)
val  lineRef : pos ref = ref 0   (* reference variable to keep track of position.
				    Typing not necessary just for clarity *)

fun updateLine n      = lineRef := !(lineRef) + n

(* Stuff done to make use of the Tokens module generated by Tiger.grm *)

type svalue        = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult     = (svalue,pos) token


fun lineRange l r = "line " ^ l
				  (*else ("line " ^ Int.toString l ^ "-" ^ Int.toString r)*)
fun error (e,l,r) = TextIO.output(TextIO.stdErr, lineRange l r ^ ":" ^ e ^ "\n")

(*
   What to return at the end of the file. Note the extra (!pos,!pos). If you have
   the clause

   %term FOO of int  | BAR

   The token module will have two functions which are

   Token.FOO : int * pos * pos
   Token.BAR : pos * pos

   Here we give the eof function for the lexer which should return the
   EOF terminal to the parser.

*)
fun eof   ()      = Tokens.EOF (!lineRef,!lineRef)

(* Some helper functions during lexing *)

fun charsToInt m (x :: xs) = charsToInt (10 * m + ord x - ord #"0") xs
  | charsToInt m []        = m

fun toSigned (#"-" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"~" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"+" :: xs) =   charsToInt 0 xs
  | toSigned xs           =   charsToInt 0 xs

val toInt        = toSigned o String.explode

val newlineCount = List.length o List.filter (fn x => x = #"\n") o String.explode

(*#line 58.1 "tiger.lex.sml"*)
end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\000\000\000\000\000\000\000\000\000\062\063\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\062\000\000\000\000\000\061\000\060\059\058\057\000\056\000\055\
\\054\054\054\054\054\054\054\054\054\054\052\051\048\047\045\000\
\\000\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\040\
\\000\004\035\004\033\027\024\004\004\021\004\004\018\004\004\004\
\\004\004\004\004\013\004\010\005\004\004\004\000\003\000\000\000\
\\000"
),
 (4, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\004\
\\000\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\000\
\\000"
),
 (5, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\004\
\\000\004\004\004\004\004\004\004\006\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\000\
\\000"
),
 (6, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\004\
\\000\004\004\004\004\004\004\004\004\007\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\000\
\\000"
),
 (7, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\004\
\\000\004\004\004\004\004\004\004\004\004\004\004\008\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\000\
\\000"
),
 (8, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\004\
\\000\004\004\004\004\009\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\000\
\\000"
),
 (10, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\004\
\\000\011\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\000\
\\000"
),
 (11, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\004\
\\000\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\012\004\004\004\004\004\004\004\004\000\000\000\000\000\
\\000"
),
 (13, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\004\
\\000\004\004\004\004\004\004\004\015\004\004\004\004\004\004\014\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\000\
\\000"
),
 (15, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\004\
\\000\004\004\004\004\016\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\000\
\\000"
),
 (16, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\004\
\\000\004\004\004\004\004\004\004\004\004\004\004\004\004\017\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\000\
\\000"
),
 (18, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\004\
\\000\004\004\004\004\019\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\000\
\\000"
),
 (19, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\004\
\\000\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\020\004\004\004\004\004\004\000\000\000\000\000\
\\000"
),
 (21, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\004\
\\000\004\004\004\004\004\023\004\004\004\004\004\004\004\022\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\000\
\\000"
),
 (24, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\004\
\\000\004\004\004\004\004\004\004\004\004\004\004\004\004\004\025\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\000\
\\000"
),
 (25, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\004\
\\000\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\026\004\004\004\004\004\004\004\004\000\000\000\000\000\
\\000"
),
 (27, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\004\
\\000\004\004\004\004\004\004\004\004\004\004\004\030\004\028\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\000\
\\000"
),
 (28, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\004\
\\000\004\004\004\029\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\000\
\\000"
),
 (30, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\004\
\\000\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\031\004\004\004\004\004\004\004\000\000\000\000\000\
\\000"
),
 (31, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\004\
\\000\004\004\004\004\032\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\000\
\\000"
),
 (33, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\004\
\\000\004\004\004\004\004\004\004\004\004\004\004\004\004\004\034\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\000\
\\000"
),
 (35, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\004\
\\000\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\036\004\004\004\004\004\004\004\004\000\000\000\000\000\
\\000"
),
 (36, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\004\
\\000\004\004\004\004\037\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\000\
\\000"
),
 (37, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\004\
\\000\038\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\000\
\\000"
),
 (38, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\000\000\000\000\000\000\
\\000\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\004\
\\000\004\004\004\004\004\004\004\004\004\004\039\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\000\000\000\000\000\
\\000"
),
 (40, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\041\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (41, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\042\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (42, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\043\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (43, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\044\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (45, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\046\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (48, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\050\049\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (52, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\053\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (54, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\054\054\054\054\054\054\054\054\054\054\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (62, 
"\000\000\000\000\000\000\000\000\000\062\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\062\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (63, 
"\000\000\000\000\000\000\000\000\000\064\063\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\064\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
(0, "")]
fun f x = x 
val s = map f (rev (tl (rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i: int) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [(N 32)], trans = 0},
{fin = [(N 103)], trans = 4},
{fin = [(N 103)], trans = 5},
{fin = [(N 103)], trans = 6},
{fin = [(N 103)], trans = 7},
{fin = [(N 103)], trans = 8},
{fin = [(N 54),(N 103)], trans = 4},
{fin = [(N 103)], trans = 10},
{fin = [(N 103)], trans = 11},
{fin = [(N 95),(N 103)], trans = 4},
{fin = [(N 103)], trans = 13},
{fin = [(N 64),(N 103)], trans = 4},
{fin = [(N 103)], trans = 15},
{fin = [(N 103)], trans = 16},
{fin = [(N 74),(N 103)], trans = 4},
{fin = [(N 103)], trans = 18},
{fin = [(N 103)], trans = 19},
{fin = [(N 84),(N 103)], trans = 4},
{fin = [(N 103)], trans = 21},
{fin = [(N 87),(N 103)], trans = 4},
{fin = [(N 44),(N 103)], trans = 4},
{fin = [(N 103)], trans = 24},
{fin = [(N 103)], trans = 25},
{fin = [(N 61),(N 103)], trans = 4},
{fin = [(N 103)], trans = 27},
{fin = [(N 103)], trans = 28},
{fin = [(N 91),(N 103)], trans = 4},
{fin = [(N 103)], trans = 30},
{fin = [(N 103)], trans = 31},
{fin = [(N 69),(N 103)], trans = 4},
{fin = [(N 103)], trans = 33},
{fin = [(N 57),(N 103)], trans = 4},
{fin = [(N 103)], trans = 35},
{fin = [(N 103)], trans = 36},
{fin = [(N 103)], trans = 37},
{fin = [(N 103)], trans = 38},
{fin = [(N 80),(N 103)], trans = 4},
{fin = [], trans = 40},
{fin = [], trans = 41},
{fin = [], trans = 42},
{fin = [], trans = 43},
{fin = [(N 103)], trans = 0},
{fin = [(N 36)], trans = 45},
{fin = [(N 25)], trans = 0},
{fin = [(N 19)], trans = 0},
{fin = [(N 34)], trans = 48},
{fin = [(N 28)], trans = 0},
{fin = [(N 22)], trans = 0},
{fin = [(N 38)], trans = 0},
{fin = [], trans = 52},
{fin = [(N 41)], trans = 0},
{fin = [(N 9)], trans = 54},
{fin = [(N 17)], trans = 0},
{fin = [(N 13)], trans = 0},
{fin = [(N 11)], trans = 0},
{fin = [(N 15)], trans = 0},
{fin = [(N 48)], trans = 0},
{fin = [(N 46)], trans = 0},
{fin = [(N 30)], trans = 0},
{fin = [(N 2)], trans = 62},
{fin = [(N 6)], trans = 63},
{fin = [], trans = 63}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val INITIAL = STARTSTATE 1;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

structure YYPosInt : INTEGER = Int
fun makeLexer yyinput =
let	val yygone0= YYPosInt.fromInt ~1
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yybufpos = ref 1		(* location of next character to use *)
	val yygone = ref yygone0	(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex () : Internal.result =
let fun continue() = lex() in
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let fun yymktext() = substring(!yyb,i0,i-i0)
			     val yypos = YYPosInt.+(YYPosInt.fromInt i0, !yygone)
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  103 => let val yytext=yymktext() in (*#line 92.19 "tiger.lex"*)Tokens.IDENTIFIER (yytext,!lineRef, !lineRef) (*#line 580.1 "tiger.lex.sml"*)
 end
| 11 => ((*#line 64.19 "tiger.lex"*) Tokens.PLUS  (!lineRef,!lineRef) (*#line 582.1 "tiger.lex.sml"*)
)
| 13 => ((*#line 65.19 "tiger.lex"*) Tokens.MINUS  (!lineRef,!lineRef) (*#line 584.1 "tiger.lex.sml"*)
)
| 15 => ((*#line 66.19 "tiger.lex"*) Tokens.MUL (!lineRef,!lineRef) (*#line 586.1 "tiger.lex.sml"*)
)
| 17 => ((*#line 67.19 "tiger.lex"*) Tokens.DIV (!lineRef,!lineRef) (*#line 588.1 "tiger.lex.sml"*)
)
| 19 => ((*#line 68.19 "tiger.lex"*) Tokens.EQ (!lineRef,!lineRef) (*#line 590.1 "tiger.lex.sml"*)
)
| 2 => ((*#line 61.19 "tiger.lex"*) lex() (*#line 592.1 "tiger.lex.sml"*)
)
| 22 => ((*#line 69.20 "tiger.lex"*) Tokens.LTEQ (!lineRef,!lineRef) (*#line 594.1 "tiger.lex.sml"*)
)
| 25 => ((*#line 70.20 "tiger.lex"*) Tokens.GTEQ (!lineRef,!lineRef) (*#line 596.1 "tiger.lex.sml"*)
)
| 28 => ((*#line 71.20 "tiger.lex"*) Tokens.NTEQ (!lineRef,!lineRef) (*#line 598.1 "tiger.lex.sml"*)
)
| 30 => ((*#line 72.19 "tiger.lex"*) Tokens.AND (!lineRef,!lineRef) (*#line 600.1 "tiger.lex.sml"*)
)
| 32 => ((*#line 73.19 "tiger.lex"*) Tokens.OR (!lineRef,!lineRef) (*#line 602.1 "tiger.lex.sml"*)
)
| 34 => ((*#line 74.19 "tiger.lex"*) Tokens.LT (!lineRef,!lineRef) (*#line 604.1 "tiger.lex.sml"*)
)
| 36 => ((*#line 75.19 "tiger.lex"*) Tokens.GT (!lineRef,!lineRef) (*#line 606.1 "tiger.lex.sml"*)
)
| 38 => ((*#line 76.19 "tiger.lex"*) Tokens.SEMICOLON (!lineRef,!lineRef) (*#line 608.1 "tiger.lex.sml"*)
)
| 41 => ((*#line 77.16 "tiger.lex"*) Tokens.ASSIGN (!lineRef,!lineRef) (*#line 610.1 "tiger.lex.sml"*)
)
| 44 => ((*#line 78.16 "tiger.lex"*) Tokens.IF  (yypos,yypos+2) (*#line 612.1 "tiger.lex.sml"*)
)
| 46 => ((*#line 79.15 "tiger.lex"*) Tokens.LPAREN (!lineRef,!lineRef) (*#line 614.1 "tiger.lex.sml"*)
)
| 48 => ((*#line 80.15 "tiger.lex"*) Tokens.RPAREN (!lineRef,!lineRef) (*#line 616.1 "tiger.lex.sml"*)
)
| 54 => ((*#line 81.19 "tiger.lex"*) Tokens.WHILE (!lineRef,!lineRef) (*#line 618.1 "tiger.lex.sml"*)
)
| 57 => ((*#line 82.16 "tiger.lex"*) Tokens.DO (!lineRef,!lineRef) (*#line 620.1 "tiger.lex.sml"*)
)
| 6 => let val yytext=yymktext() in (*#line 62.19 "tiger.lex"*) updateLine (newlineCount yytext) ;lex()(*Tokens.NEWLINE (!lineRef, !lineRef)*)(*#line 622.1 "tiger.lex.sml"*)
 end
| 61 => ((*#line 83.17 "tiger.lex"*) Tokens.FOR (!lineRef,!lineRef) (*#line 624.1 "tiger.lex.sml"*)
)
| 64 => ((*#line 84.16 "tiger.lex"*) Tokens.TO (!lineRef,!lineRef) (*#line 626.1 "tiger.lex.sml"*)
)
| 69 => ((*#line 85.18 "tiger.lex"*) Tokens.ELSE (!lineRef,!lineRef) (*#line 628.1 "tiger.lex.sml"*)
)
| 74 => ((*#line 86.18 "tiger.lex"*) Tokens.THEN (!lineRef,!lineRef) (*#line 630.1 "tiger.lex.sml"*)
)
| 80 => ((*#line 87.19 "tiger.lex"*) Tokens.BREAK (!lineRef,!lineRef) (*#line 632.1 "tiger.lex.sml"*)
)
| 84 => ((*#line 88.17 "tiger.lex"*) Tokens.LET (!lineRef,!lineRef) (*#line 634.1 "tiger.lex.sml"*)
)
| 87 => ((*#line 89.16 "tiger.lex"*) Tokens.IN (!lineRef,!lineRef) (*#line 636.1 "tiger.lex.sml"*)
)
| 9 => let val yytext=yymktext() in (*#line 63.18 "tiger.lex"*) Tokens.CONST (toInt yytext, !lineRef, !lineRef) (*#line 638.1 "tiger.lex.sml"*)
 end
| 91 => ((*#line 90.17 "tiger.lex"*) Tokens.END (!lineRef,!lineRef) (*#line 640.1 "tiger.lex.sml"*)
)
| 95 => ((*#line 91.17 "tiger.lex"*) Tokens.VAR (!lineRef,!lineRef) (*#line 642.1 "tiger.lex.sml"*)
)
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Vector.sub(Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof ()
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := substring(!yyb,i0,l-i0)^newchars;
		     yygone := YYPosInt.+(!yygone, YYPosInt.fromInt i0);
		     yybl := size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord(CharVector.sub(!yyb,l))
		val NewChar = if NewChar<128 then NewChar else 128
		val NewState = Char.ord(CharVector.sub(trans,NewChar))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
end
  in lex
  end
end
