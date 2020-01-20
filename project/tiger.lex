type lexresult = Token 

fun eof () = EOF

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
comment = ([/])([\*])(.|[^.]*)([\*])([/]);
%%
{whitespace}+     => (lex() );
"#".*\n           => (lex() );
"array"               => ( (key  ARRAY    ));
"if"               => ( (key  IF) );
"else"               => ( (key  ELSE) );
"then"               => ( (key  THEN) );
"while"               => ( (key  WHILE) );
"for"               => ( (key  FOR) );
{id}|"_main"			=>((var yytext ));
{comment}		=>( (comment yytext));
 
