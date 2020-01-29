structure Ast =
struct
  datatype Expr = Const of int
                 | Op of Expr * BinOp * Expr
            and BinOp = Plus
                        |Minus
                        |Mul
                        |Divide
    fun plus  a b = Op (a, Plus, b)
    fun minus a b = Op (a, Minus, b)
    fun mul   a b = Op (a, Mul, b)
    fun divide a b = Op(a, Divide, b)
end