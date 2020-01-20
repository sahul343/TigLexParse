datatype keywords = ARRAY | IF | THEN | ELSE | WHILE | FOR 
datatype Token = key of keywords | EOF | var of string | comment of string


