signature Tiger_TOKENS =
sig
type ('a,'b) token
type svalue
val UMINUS:  'a * 'a -> (svalue,'a) token
val VAR:  'a * 'a -> (svalue,'a) token
val END:  'a * 'a -> (svalue,'a) token
val IN:  'a * 'a -> (svalue,'a) token
val LET:  'a * 'a -> (svalue,'a) token
val SEMICOLON:  'a * 'a -> (svalue,'a) token
val IDENTIFIER: (string) *  'a * 'a -> (svalue,'a) token
val BREAK:  'a * 'a -> (svalue,'a) token
val TO:  'a * 'a -> (svalue,'a) token
val FOR:  'a * 'a -> (svalue,'a) token
val DO:  'a * 'a -> (svalue,'a) token
val WHILE:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val ASSIGN:  'a * 'a -> (svalue,'a) token
val THEN:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
val NEWLINE:  'a * 'a -> (svalue,'a) token
val NTEQ:  'a * 'a -> (svalue,'a) token
val OR:  'a * 'a -> (svalue,'a) token
val AND:  'a * 'a -> (svalue,'a) token
val GT:  'a * 'a -> (svalue,'a) token
val GTEQ:  'a * 'a -> (svalue,'a) token
val LTEQ:  'a * 'a -> (svalue,'a) token
val EQ:  'a * 'a -> (svalue,'a) token
val LT:  'a * 'a -> (svalue,'a) token
val DIV:  'a * 'a -> (svalue,'a) token
val MUL:  'a * 'a -> (svalue,'a) token
val MINUS:  'a * 'a -> (svalue,'a) token
val PLUS:  'a * 'a -> (svalue,'a) token
val CONST: (int) *  'a * 'a -> (svalue,'a) token
end
signature Tiger_LRVALS=
sig
structure Tokens : Tiger_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
