structure Ast =
struct 
  datatype BinOp = Plus
                        |Minus
                        |Mul
                        |Divide
  datatype Expr = Const of int
	         | Var of string
                 | Op of Expr * BinOp * Expr
		 | Assign of Expr*Expr
		 | IF
		 | WHILE of Expr*Expr
		 | FOR of Expr*Expr*Expr*Expr
		 | OPENIF of (Expr*Expr)
		 | CLOSEDIF of (Expr*Expr*Expr)
    fun plus  a b = Op (a, Plus, b)
    fun minus a b = Op (a, Minus, b)
    fun mul   a b = Op (a, Mul, b)
    fun divide a b = Op(a, Divide, b)
    fun assign a b = Assign(a, b)
    fun While a b = WHILE(a, b)
    fun For a b c d = FOR (a, b, c, d)
    fun Openif a b = OPENIF(a, b)
    fun Closedif a b c = CLOSEDIF (a, b, c)
end
