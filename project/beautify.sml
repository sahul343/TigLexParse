val blue = "\027[1;34m"
val white = "\027[0m"
val yellow = "\027[0;33m"
structure beautify =
struct
  fun put space = if (space = 0) then ("") else ("  "^(put (space-1)))
  fun indent space (Ast.Op(a, oper, b)) = "("^(indent 0 a)^(Ast.binOPtoString oper)^(indent 0 b)^")"
    |indent space (Ast.Const x) = (put space)^(Int.toString x)
    |indent space (Ast.Var x) 	= (put space) ^ yellow^x^white
    |indent space  (Ast.Assign (x, y) ) = (put space) ^ (indent space x)^" := "^(indent space y)
    |indent space  (Ast.WHILE (x,y) )   = (put space)^blue^"while"^white^(indent space x)^blue^" do"^white^"\n"^(put (space+1))^(indent space y)
    |indent space  (Ast.FOR   (a, b, c, d) )= (put space)^blue^"for "^white^(indent 0 a)^" := "^(indent 0 b)^blue^" to"^white^(indent space c)^blue^" do"^white^" \n"^(indent (space+1) d)
    |indent space (Ast.OPENIF (a,b) ) =(put space)^blue^"if"^white^" " ^ (indent space a) ^blue^" then"^white^" (\n"^(indent (space+1) b)^"\n"^(put space)^")\n" 
    |indent space (Ast.CLOSEDIF (a,b, c) ) = (indent space (Ast.OPENIF(a, b)))^(put space)^blue^"else"^white^" (\n" ^(indent (space+1) c)^"\n"^(put space)^")\n"
    |indent space (Ast.BREAK) = (put space)^blue^"break"^white^"\n"
    |indent space (Ast.LET(a, b) ) = (put space)^blue^"let"^white^"\n"^(indentdeclist (space+1) a)^blue^"\nin"^white^"\n"^(indentlist (space+1) b)^(put space)^blue^"end"^white^"\n"
and
      indentdec space (Ast.VarDec(a, b)) = (put space)^"var "^(indent space a)^" := "^(indent space b)
and 
      indentdeclist space []      = ""
     |indentdeclist space (x::xs) = (indentdec space x)^"\n"^(indentdeclist space xs)
and 
      indentlist space []      = ""
     |indentlist space (x::xs) = (indent space x)^"\n"^(indentlist space xs)
end
