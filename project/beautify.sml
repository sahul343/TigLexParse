structure beautify =
struct
  fun indent space (Ast.Op(a, Ast.Plus, b)) = (indent space a)^"+"^(indent space b)
    |indent space (Ast.Op(a, Ast.Minus, b)) = (indent space a)^"-"^(indent space b)
    |indent space (Ast.Op(a, Ast.Mul, b)) = (indent space a)^"*"^(indent space b)
    |indent space (Ast.Op(a, Ast.Divide, b)) = (indent space a)^"/"^(indent space b)
    |indent space (Ast.Const x) = (Int.toString x)
  fun indentlist space []      = ""
     |indentlist space (x::xs) = (indent space x)^(indentlist space xs)
end