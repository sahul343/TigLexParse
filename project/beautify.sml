structure beautify =
struct
  fun put space = if (space = 0) then ("") else ("  "^(put (space-1)))
  fun indent space (Ast.Op(a, oper, b)) = "("^(indent 0 a)^(Ast.binOPtoString oper)^(indent 0 b)^")"
    |indent space (Ast.Const x) = (put space)^(Int.toString x)
    |indent space (Ast.Var x) 	= (put space) ^ "\027[0;33m"^x^"\027[0m"
    |indent space  (Ast.Assign (x, y) ) = (put space) ^ (indent space x)^" := "^(indent space y)
    |indent space  (Ast.WHILE (x,y) )   = (put space)^"\027[1;34mwhile\027[0m "^(indent space x)^" \027[1;34mdo\027[0m\n"^(put (space+1))^(indent space y)
    |indent space  (Ast.FOR   (a, b, c, d) )= (put space)^"\027[1;34mfor\027[0m "^(indent 0 a)^" := "^(indent 0 b)^" \027[1;34mto\027[0m "^(indent space c)^" \027[1;34mdo\027[0m \n"^(indent (space+1) d)
    |indent space (Ast.OPENIF (a,b) ) =(put space)^"\027[1;34mif\027[0m " ^ (indent space a) ^" \027[1;34mthen\027[0m (\n"^(indent (space+1) b)^"\n"^(put space)^")\n" 
    |indent space (Ast.CLOSEDIF (a,b, c) ) = (indent space (Ast.OPENIF(a, b)))^(put space)^"\027[1;34melse\027[0m (\n" ^(indent (space+1) c)^"\n"^(put space)^")\n"
    |indent space (Ast.BREAK) = (put space)^"\027[1;34mbreak\027[0m\n"
    |indent space (Ast.LET(a, b) ) = (put space)^"\027[1;34mlet\027[0m\n"^(indentdeclist (space+1) a)^"\027[1;34m\nin\027[0m\n"^(indentlist (space+1) b)^(put space)^"\027[1;34mend\027[0m\n"
and
      indentdec space (Ast.VarDec(a, b)) = (put space)^"var "^(indent space a)^" := "^(indent space b)
and 
      indentdeclist space []      = ""
     |indentdeclist space (x::xs) = (indentdec space x)^"\n"^(indentdeclist space xs)
and 
      indentlist space []      = ""
     |indentlist space (x::xs) = (indent space x)^"\n"^(indentlist space xs)
end
