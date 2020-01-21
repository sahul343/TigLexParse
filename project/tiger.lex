type lexresult = Token 

fun eof () = EOF;

fun charsToInt m (x :: xs) = charsToInt (10 * m + ord x - ord #"0") xs
  | charsToInt m []        = m

fun toSigned (#"-" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"~" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"+" :: xs) =   charsToInt 0 xs
  | toSigned xs           =   charsToInt 0 xs

val toInt  = toSigned o String.explode

%%
%structure TIGHlex
whitespace=[\ \r\t\n];
digit=[0-9];
str = [a-z_A-Z]+;
id  = [a-zA-Z]([a-z_A-Z1-9])*;
comment = "/*"([^*]|\*+[^*/])*\*+"/";
%%
{whitespace}+     => (lex() );
"#".*\n           => (lex() );
"array"     => (key ARRAY);
"if"      => (key IF);
"then"      => (key THEN);
"else"      => (key ELSE);
"while"     => (key WHILE);
"for"     => (key FOR);
"to"      => (key TO);
"do"      => (key DO);
"let"     => (key LET);
"in"      => (key IN);
"end"     => (key END);
"of"      => (key OF);
"break"     => (key BREAK);
"nil"     => (key NIL);
"function"    => (key FUNCTION);
"var"     => (key VAR);
"type"      => (key TYPE);
"import"    => (key IMPORT);
"primitive"   => (key PRIMITIVE);
"class"  => ( object CLASS);
"extends" => ( object EXTENDS);
"method" => ( object METHOD);
"new" => ( object NEW);
{id}|"_main"			=>((var yytext ));
{comment}		=>( (comment yytext));
 
