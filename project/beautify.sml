structure beautify =
struct
  fun indent space (Ast.Op(a, Ast.Plus, b)) = "("^(indent space a)^"+"^(indent space b)^")"
    |indent space (Ast.Op(a, Ast.Minus, b)) = "("^(indent space a)^"-"^(indent space b)^")"
    |indent space (Ast.Op(a, Ast.Mul, b)) = (indent space a)^"*"^(indent space b)
    |indent space (Ast.Op(a, Ast.Divide, b)) = (indent space a)^"/"^(indent space b)
    |indent space (Ast.Const x) = (Int.toString x)
    |indent space (Ast.Var x) 	= x
    |indent space  (Ast.IF ) = "if"
    |indent space  (Ast.Assign (x, y) ) = (indent space x)^" := "^(indent space y)
    |indent space  (Ast.WHILE (x,y) )   = "while "^(indent space x)^" do\n"^(indent space y)
    |indent space  (Ast.FOR   (a, b, c, d) )= "for "^(indent space a)^" := "^(indent space b)^" to "^(indent space c)^" do "^(indent space d)



 fun indentlist space []      = ""
     |indentlist space (x::xs) = (indent space x)^"\n"^(indentlist space xs)
end
