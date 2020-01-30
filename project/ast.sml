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
    fun plus  a b = Op (a, Plus, b)
    fun minus a b = Op (a, Minus, b)
    fun mul   a b = Op (a, Mul, b)
    fun divide a b = Op(a, Divide, b)
    fun assign a b = Assign(a, b)
end
