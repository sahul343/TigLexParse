structure beautify =
struct
  fun put space = if (space = 0) then ("") else ("  "^(put (space-1)))
  fun indent space (Ast.Op(a, Ast.Plus, b)) = "("^(indent space a)^"+"^(indent space b)^")"
    |indent space (Ast.Op(a, Ast.Minus, b)) = "("^(indent space a)^"-"^(indent space b)^")"
    |indent space (Ast.Op(a, Ast.Mul, b)) = (indent space a)^"*"^(indent space b)
    |indent space (Ast.Op(a, Ast.Divide, b)) = (indent space a)^"/"^(indent space b)
    |indent space (Ast.Const x) = (put space)^(Int.toString x)
    |indent space (Ast.Var x) 	= (put space) ^ x
    |indent space  (Ast.Assign (x, y) ) = (put space) ^ (indent space x)^" := "^(indent space y)
    |indent space  (Ast.WHILE (x,y) )   = (put space)^"while "^(indent space x)^" do\n"^(put (space+1))^(indent space y)
    |indent space  (Ast.FOR   (a, b, c, d) )= (put space)^"for "^(indent space a)^" := "^(indent space b)^" to "^(indent space c)^" do \n"^(indent (space+1) d)
    |indent space (Ast.OPENIF (a,b) ) =(put space)^"if " ^ (indent space a) ^" then (\n "^(indent (space+1) b)^"\n"^(put space)^" )\n" 
    |indent space (Ast.CLOSEDIF (a,b, c) ) = (indent space (Ast.OPENIF(a, b)))^(put space)^"else (\n" ^(indent (space+1) c)^"\n"^(put space)^")\n"



 fun indentlist space []      = ""
     |indentlist space (x::xs) = (indent space x)^"\n"^(indentlist space xs)
end
