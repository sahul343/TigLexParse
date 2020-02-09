val blue = "\027[1;34m"
val white = "\027[0m"
val yellow = "\027[0;33m"
structure beautify =
struct
  fun put s = if (s = 0) then ("") else ("  "^(put (s-1)))
  fun indent s (Ast.Op(a, oper, b)) = "("^(indent 0 a)^(Ast.binOPtoString oper)^(indent 0 b)^")"
    |indent s (Ast.Neg x)   = "( ~"^(indent 0 x)^")"
    |indent s (Ast.Closed x) = (put s)^"(\n"^(indentlist (s+1) x)^"\n"^(put s)^")\n"
    |indent s (Ast.Const x) = (put s)^(Int.toString x)
    |indent s (Ast.Quote x) = (put s)^x
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
    |indent s (Ast.Object a)       = (put s)^"new "^a
    |indent s (Ast.FunCall (a, b) )       = (put s)^a^"("^(indentlist s b)^")"
    |indent s (Ast.MethodCall(a, b, c))       = (put s)^"("^(indent s
    a)^"."^b^(indentlist s c)^")"
    |indent s (Ast.Record (a, b) )  = 	let
						fun printRecbody [] 	= "" 
						|printRecbody ((a,b)::[]) = a^" = "^(indent 0 b)
						|printRecbody ((a,b)::x )  = a^" = "^(indent 0 b)^", "^(printRecbody x)		
					in
						a^"{"^(printRecbody b)^"}\n"	
					end					
and
      indentdec s (Ast.VariableDec a) = (pvardec s a)
      |indentdec s (Ast.Import a) = (put s)^"import "^a^" \n "
      |indentdec s (Ast.FunctionDec (a,b,c)) = (put s)^"function "^a^"( "^(indenttyfield b)^") = "^(indent s c)^" \n "

      |indentdec s (Ast.FunctionDecType (a,b,c,d)) = (put s)^"function "^a^"("^(indenttyfield b)^"): "^c^ " = "^(indent s d)^" \n "
      |indentdec s (Ast.PrimitiveDec (a,b)) = (put s)^"primitive "^a^"("^(indenttyfield b)^")\n "
      |indentdec s (Ast.PrimitiveDecType (a,b,c)) = (put s)^"primitive "^a^"("^(indenttyfield b)^"): "^c^"\n"
      |indentdec s (Ast.TypeDec (a, b) ) = (put s)^"type "^a^" = "^(printty s b)
      |indentdec s (Ast.ClassDec (a,b) ) = (put s)^"class "^a^"{ \n"^(indentclasslist (s+1) b)^(put s)^"\n"
      |indentdec s (Ast.ClassDecType (a,b,c) ) = (put s)^"class "^a^" extends "^b^"{ \n"^(indentclasslist (s+1) c)^(put s)^"\n"
and 

	indenttyfield (Ast.Tyfield a) = let
						fun p [] = " "
						|p ((a,b)::xs) = a^" : "^b^(p xs)
					in
						(p a)
					end
and
	printty s (Ast.NameTy a) 		= a
	|printty s (Ast.RecordTy a) 	= "{"^(indenttyfield a)^"}"
	|printty s (Ast.ArrayTy a) 	= "array of "^a
	|printty s (Ast.ClassDefCan a)	= "class { \n"^(indentclasslist (s+1) a)^(put s)^"\n"
	|printty s (Ast.ClassDefCanType (a,b) ) = (put s)^"class  extends "^a^"{ \n"^(indentclasslist (s+1) b)^(put s)^"\n"
and
      indentdeclist s []      = ""
     |indentdeclist s (x::xs) = (indentdec s x)^"\n"^(indentdeclist s xs)
and 
      indentlist s []      = ""
     |indentlist s (x::xs) = (indent s x)^"\n"^(indentlist s xs)
  and
      classfield s (Ast.MethodDec (a,b,c) ) = (put s)^"method "^a^" ( "^(indenttyfield  b)^" ) = "^(indent 0 c)  
     | classfield s (Ast.MethodDecType (a,b,c,d) ) = (put s)^"method "^a^"( "^(indenttyfield b)^" ) : "^c^" = "^(indent 0 d)
     | classfield s (Ast.ClassAttribute a) = (put s)^(pvardec 0 a)    

and
	indentclasslist s []      = ""
     |indentclasslist s (x::xs) = (classfield s x)^"\n"^(indentclasslist s xs)
and
	pvardec s (Ast.VarDec(a, b)) = (put s)^"var "^a^" := "^(indent s b)
	|pvardec s (Ast.VarDecType(a, b,c)) = (put s)^"var "^a^": "^b^" := "^(indent s c)
and
  pretty s (Ast.Foo a)  = (indent s a)
 |pretty s (Ast.Bar a) = (indentdeclist s a)

     
end
