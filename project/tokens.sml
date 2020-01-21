datatype keywords =ARRAY | IF | THEN | ELSE | WHILE | FOR | TO | DO | LET | IN | END | OF | BREAK | NIL | FUNCTION | VAR | TYPE | IMPORT | PRIMITIVE

datatype objectkeywords = CLASS
            | EXTENDS
            | METHOD
            | NEW
datatype Token = key of keywords|object of objectkeywords | EOF | var of string | comment of string


