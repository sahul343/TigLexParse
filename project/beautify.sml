val blue = "\027[1;34m"
val white = "\027[0m"
val yellow = "\027[0;33m"
structure beautify =
struct
  fun put s = if (s = 0) then ("") else ("  "^(put (s-1)))
  fun indent s (Ast.Op(a, oper, b)) = "("^(indent 0 a)^(Ast.binOPtoString oper)^(indent 0 b)^")"
    |indent s (Ast.Const x) = (put s)^(Int.toString x)
    |indent s  (Ast.Assign (x, y) ) = (put s) ^(indent s x)^" := "^(indent s y)
    |indent s  (Ast.WHILE (x,y) )   = (put s)^blue^"while"^white^(indent s x)^blue^" do"^white^"\n"^(put (s+1))^(indent s y)
    |indent s  (Ast.FOR   (a, b, c, d) )= (put s)^blue^"for "^white^a^" := "^(indent 0 b)^blue^" to"^white^(indent s c)^blue^" do"^white^" \n"^(indent (s+1) d)
    |indent s (Ast.OPENIF (a,b) ) =(put s)^blue^"if"^white^" " ^ (indent s a) ^blue^" then"^white^" (\n"^(indent (s+1) b)^"\n"^(put s)^")\n" 
    |indent s (Ast.CLOSEDIF (a,b, c) ) = (indent s (Ast.OPENIF(a, b)))^(put s)^blue^"else"^white^" (\n" ^(indent (s+1) c)^"\n"^(put s)^")\n"
    |indent s (Ast.BREAK) = (put s)^blue^"break"^white^"\n"
    |indent s (Ast.LET(a, b) ) = (put s)^blue^"let"^white^"\n"^(indentdeclist (s+1) a)^blue^"\nin"^white^"\n"^(indentlist (s+1) b)^(put s)^blue^"end"^white^"\n"
    | indent s (Ast.Name x) = x
    |indent s (Ast.Method(x,y) ) = (indent s x)^"."^y	
    |indent s (Ast.Access(x,y) ) = (indent s x)^"["^(indent 0 y)^"]"
    |indent s (Ast.NIL ) = "nil"
    |indent s (Ast.Array (a,b,c) ) = a^"["^(indent s b)^"]"^" of "^(indent s c)
    |indent s (Ast.Record (a, b) )  = 	let
						fun printRecbody [] 	= "" 
						|printRecbody ((a,b)::[]) = a^" = "^(indent 0 b)
						|printRecbody ((a,b)::x )  = a^" = "^(indent 0 b)^","^(printRecbody x)		
					in
						a^"{ "^(printRecbody b)^" }\n"	
					end					
and
      indentdec s (Ast.VarDec(a, b)) = (put s)^"var "^a^" := "^(indent s b)
and 
      indentdeclist s []      = ""
     |indentdeclist s (x::xs) = (indentdec s x)^"\n"^(indentdeclist s xs)
and 
      indentlist s []      = ""
     |indentlist s (x::xs) = (indent s x)^"\n"^(indentlist s xs)
     
end
