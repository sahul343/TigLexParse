(* Internal datatypes and functions required by the lexer *)
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

%%

%header (functor TigerLexFun(structure Tokens : Tiger_TOKENS));
ws    = [\ \t];
digit = [0-9]+;
letter = [a-zA-Z];
id  = [a-zA-Z]([a-z_A-Z1-9])*;
%%
{ws}+         => ( lex() );
\n({ws}*\n)*  => ( updateLine (newlineCount yytext) ;lex()(*Tokens.NEWLINE (!lineRef, !lineRef)*));
{digit}      => ( Tokens.CONST (toInt yytext, !lineRef, !lineRef) );
"+"           => ( Tokens.PLUS  (!lineRef,!lineRef) );
"-"           => ( Tokens.MINUS  (!lineRef,!lineRef) );
"*"           => ( Tokens.MUL (!lineRef,!lineRef) );
"/"           => ( Tokens.DIV (!lineRef,!lineRef) );
"="           => ( Tokens.EQ (!lineRef,!lineRef) );
"<="           => ( Tokens.LTEQ (!lineRef,!lineRef) );
">="           => ( Tokens.GTEQ (!lineRef,!lineRef) );
"<>"           => ( Tokens.NTEQ (!lineRef,!lineRef) );
"&"           => ( Tokens.AND (!lineRef,!lineRef) );
"|"           => ( Tokens.OR (!lineRef,!lineRef) );
"<"           => ( Tokens.LT (!lineRef,!lineRef) );
">"           => ( Tokens.GT (!lineRef,!lineRef) );
";"           => ( Tokens.SEMICOLON (!lineRef,!lineRef) );
":="	      => ( Tokens.ASSIGN (!lineRef,!lineRef) );
"if"	      => ( Tokens.IF  (yypos,yypos+2) );
"("	      => ( Tokens.LPAREN (!lineRef,!lineRef) );
")"	      => ( Tokens.RPAREN (!lineRef,!lineRef) );
"while"	      => ( Tokens.WHILE (!lineRef,!lineRef) );
"do"	      => ( Tokens.DO (!lineRef,!lineRef) );
"for"	      => ( Tokens.FOR (!lineRef,!lineRef) );
"to"	      => ( Tokens.TO (!lineRef,!lineRef) );
"else"	      => ( Tokens.ELSE (!lineRef,!lineRef) );
"then"	      => ( Tokens.THEN (!lineRef,!lineRef) );
"break"	      => ( Tokens.BREAK (!lineRef,!lineRef) );
"let"	      => ( Tokens.LET (!lineRef,!lineRef) );
"in"	      => ( Tokens.IN (!lineRef,!lineRef) );
"end"	      => ( Tokens.END (!lineRef,!lineRef) );
"var"	      => ( Tokens.VAR (!lineRef,!lineRef) );
{id}|"_main"  => (Tokens.IDENTIFIER (yytext,!lineRef, !lineRef) );
