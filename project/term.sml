fun ws (1,TAB) = "\t"
    | ws (n,TAB) = "\t"^ws(n-1,TAB)
    | ws (1,SPACE) = " "
    | ws (n,SPACE) = " "^ws(n-1,SPACE)


fun tostring token = case token of
	 (key ARRAY) => "array"
        | (key IF) => "if"
        | (key THEN) => "then"
        | (key ELSE) => "else"
        | (key WHILE) => "while"
        | (key FOR) => "for"
        | (key LET) => "let"
        | (key TO) => "to"
        | (key DO) => "do"
        | (key IN) => "in"
        | (key END) => "end"
        | (key OF) => "of"
        | (key BREAK) => "break"
        | (key NIL) => "nil"
        | (key FUNCTION) => "function"
        | (key VAR) => "var"
        | (key TYPE) => "type"
        | (key IMPORT) => "import"
        | (key PRIMITIVE) => "primitive"
	| (object CLASS) => "class"
	| (object EXTENDS) => "extends"
	| (object METHOD) => "method"
	| (object NEW) => "new"
	| (sym Comma) => ","
        | (sym Colon) => ":"
        | (sym Semicolon) => ";"
        | (sym LeftB) => "("
        | (sym RightB) => ")"
        | (sym LeftSB) => "["
        | (sym RightSB) => "]"
        | (sym LeftCB) => "{"
        | (sym RightCB) => "}"
        | (sym Dot) => "."
        | (sym Plus) => "+"
        | (sym Minus) => "-"
        | (sym Mul) => "*"
        | (sym Div) => "/"
        | (sym Equal) => "="
        | (sym LTGT) => "<>"
        | (sym LT) => "<"
        | (sym LTEqual) => "<="
        | (sym GT) => ">"
        | (sym GTEqual) => ">="
        | (sym AND) => "&"
        | (sym OR) => "|"
        | (sym ColonEqual) => ":="
	| (white (x,n) ) => ws(n,x)
	| (CONST x) => Int.toString(x)




fun myprint inst = (case inst of 
	(key _) => print("\027[32m"^(tostring inst))
	|(object _) => print("\027[33m"^(tostring inst))
	|(sym _) => print("\027[36m"^(tostring inst))
	|(var str) => print("\027[31m"^str)
	|(comment str) => print("\027[34m"^str)
	|(white _) => print("\027[0m"^(tostring inst))
	|(NEWLINE) => print("\027[0m\n")
	|(CONST x) =>print("\027[0m"^(tostring inst))
	)

