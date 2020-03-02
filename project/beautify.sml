val blue = "\027[1;34m"
val white = "\027[0m"
val yellow = "\027[0;33m"
structure beautify =
struct
  fun  put s = if (s = 0) then ("") else ("  "^(put (s-1)))
  fun indent s  n (Ast.NIL ) = "nil"
    |indent s  n (Ast.Const x) = (Int.toString x)
    |indent s  n (Ast.Quote x) = x
    |indent s  n (Ast.Array (a,b,c) ) = a^"["^( indent 0 n b)^"]"^" of "^( indent 0 n c)
    |indent s  n (Ast.Record (a, b) )  = 	let
						fun printRecbody [] 	= "" 
						|printRecbody ((a,b)::[]) = a^" = "^( indent 0 n b)
						|printRecbody ((a,b)::x )  = a^" = "^( indent 0 n b)^", "^(printRecbody x)		
					in
						a^"{"^(printRecbody b)^"}\n"	
					end	
    |indent s  n (Ast.Object a)       = (put s)^"new "^a
    | indent s  n (Ast.Name x) = x
    |indent s  n (Ast.Method(x,y) ) = ( indent 0 n x)^"."^y	
    |indent s  n (Ast.Access(x,y) ) = ( indent 0 n x)^"["^( indent 0 n y)^"]"
    |indent s  n (Ast.FunCall (a, b) )       = n^(put s)^a^"("^(Parguments  b)^")"
    |indent s  n (Ast.MethodCall(a, b, c))       = n^(put s)^(indent s  n a)^"."^b^"("^(Parguments  c)^")"
    |indent s  n (Ast.Neg x)   = "( ~"^( indent 0 n x)^")"
    |indent s  n (Ast.Op(a, oper, b)) = "("^( indent 0 n a)^(Ast.binOPtoString oper)^( indent 0 n b)^")"
    |indent s  n (Ast.Closed x) = "(\n"^(indentlist (s+1) n x)^"\n"^(put s)^")"
    |indent s  n  (Ast.Assign (x, y) ) = (put s) ^( indent 0 n x)^" := "^( indent 0 n y)
    |indent s  n (Ast.OPENIF (a,b) ) =n^(put s)^blue^"if"^white^" " ^ (indent s  n a) ^blue^" then"^white^(indent (s+1) n b) 
    |indent s  n (Ast.CLOSEDIF (a,b, c) ) = n^(indent s  n (Ast.OPENIF(a, b)))^(put s)^blue^"else"^white^(indent (s+1) n c)
    |indent s  n  (Ast.WHILE (x,y) )   = (put s)^blue^"while"^white^(indent s  n x)^blue^" do"^white^"\n"^(put (s+1))^(indent s  ("\n") y)
    |indent s  n  (Ast.FOR   (a, b, c, d) )= "\n"^(put s)^blue^"for "^white^a^" := "^( indent 0 n b)^blue^" to "^white^( indent 0 n c)^blue^" do"^white^(indent (s+1) ("\n") d)
    |indent s  n (Ast.BREAK) = (put s)^blue^"break"^white
    |indent s  n (Ast.LET(a, b) ) = (put s)^blue^"let"^white^"\n"^(indentdeclist (s+1) n a)^blue^"\n"^(put s)^"in"^white^"\n"^(indentlist (s+1) n b)^"\n"^(put s)^blue^"end"^white
   				
and
      indentdec s  n (Ast.VariableDec a) = (pvardec s  n a)
      |indentdec s  n (Ast.TypeDec (a, b) ) = (put s)^"type "^a^" = "^(printty 0 ("") b)
      |indentdec s  n (Ast.ClassDec (a,b) ) = (put s)^"class "^a^"{ \n"^(indentclasslist (s+1) n b)^(put s)^"}\n"
      |indentdec s  n (Ast.ClassDecType (a,b,c) ) = (put s)^"class "^a^" extends "^b^"{ \n"^(indentclasslist (s+1) n c)^(put s)^"}\n"
      |indentdec s  n (Ast.Import a) = (put s)^"import "^a^" \n "
      |indentdec s  n (Ast.FunctionDec (a,b,c)) = (put s)^"function "^a^"("^(ptyfield b)^") = "^(indent s  n c)^" \n "
      |indentdec s  n (Ast.FunctionDecType (a,b,c,d)) = (put s)^"function "^a^"("^(ptyfield b)^"): "^c^ " = "^(indent s  n d)^" \n "
      |indentdec s  n (Ast.PrimitiveDec (a,b)) = (put s)^"primitive "^a^"("^(ptyfield b)^")\n "
      |indentdec s  n (Ast.PrimitiveDecType (a,b,c)) = (put s)^"primitive "^a^"("^(ptyfield b)^"): "^c^"\n"
and 

	Parguments [] = ""
	|Parguments (x::[]) = ( indent 0 ("") x)^""
	|Parguments (x::xs) = ( indent 0 ("") x)^", "^(Parguments xs)


and	ptyfield (Ast.Tyfield a) = let
						fun p [] = " "
						|p ((a,b)::[]) = a^" : "^b
						|p ((a,b)::xs) = a^" : "^b^" , "^(p xs)
					in
						(p a)
					end
and
	printty s  n (Ast.NameTy a) 		= a
	|printty s  n (Ast.RecordTy a) 	= "{"^(ptyfield a)^"}"
	|printty s  n (Ast.ArrayTy a) 	= "array of "^a
	|printty s  n (Ast.ClassDefCan a)	= "class { \n"^(indentclasslist (s+1) n a)^(put s)^"}\n"
	|printty s  n (Ast.ClassDefCanType (a,b) ) = "class  extends "^a^"{ \n"^(indentclasslist (s+1) n b)^(put s)^"}\n"
and
      indentdeclist s  n []      = ""
     |indentdeclist s  n (x::xs) = (indentdec s  n x)^"\n"^(indentdeclist s  n xs)
and 
      indentlist s  n []      = "\n"
     |indentlist s  n (x::[]) = n^(indent s  n x)
     |indentlist s  n (x::xs) = n^(indent s  n x)^";"^(indentlist s  ("\n") xs)
  and
      classfield s  n (Ast.MethodDec (a,b,c) ) = (put s)^"method "^a^" ( "^(ptyfield  b)^" ) = "^( indent 0 n c)  
     | classfield s  n (Ast.MethodDecType (a,b,c,d) ) = (put s)^"method "^a^"( "^(ptyfield b)^" ) : "^c^" = "^( indent 0 n d)
     | classfield s  n (Ast.ClassAttribute a) = (pvardec s  n a)    

and
	indentclasslist s  n []      = ""
     |indentclasslist s  n (x::xs) = (classfield s  n x)^"\n"^(indentclasslist s  n xs)
and
	pvardec s  n (Ast.VarDec(a, b)) = (put s)^"var "^a^" := "^(indent s  n b)
	|pvardec s  n (Ast.VarDecType(a, b,c)) = (put s)^"var "^a^": "^b^" := "^(indent s  n c)
and
  pretty s  n (Ast.Foo a)  = (indent s  n a)^"\n"
 |pretty s  n (Ast.Bar a) = (indentdeclist s  n a)

     
end
