type lexresult = Token

fun eof () = NONE

fun charsToInt m (x :: xs) = charsToInt (10 * m + ord x - ord #"0") xs
  | charsToInt m []        = m

fun toSigned (#"-" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"~" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"+" :: xs) =   charsToInt 0 xs
  | toSigned xs           =   charsToInt 0 xs

val toInt  = toSigned o String.explode


%structure TIGHlex
whitespace=[\ \r\t\n];
digit=[0-9];

%%
{whitespace}+     => (lex()  (* White spaces are ignored *) );
"#".*\n           => (lex()  (* A line comment *)           );
"array"               => (key of array);
"if"               => (key of if);
"else"               => (key of else);
"then"               => (key of then);
"while"               => (key of while);
"for"               => (key of for);

