val blue = "\027[1;34m"
val white = "\027[0m"
val yellow = "\027[0;33m"
structure beautify =
struct
  fun  put space = if (space = 0) then ("") else ("  "^(put (space-1)))
  fun add s (Ast.NIL ) = "nil"
    |add s (Ast.Const x) = (Int.toString x)
    |add s (Ast.Quote x) = x
    |add s (Ast.Array (a,b,c) ) = a^"["^( add 0b)^"]"^" of "^( add 0c)
    |add s (Ast.Record (a, b) )  = 	let
						fun printRecbody [] 	= ""
						|printRecbody ((a,b)::[]) = a^" = "^( add 0b)
						|printRecbody ((a,b)::x )  = a^" = "^( add 0b)^", "^(printRecbody x)	
					in
						a^"{"^(printRecbody b)^"}"^ "\n"	
					end	
    |add s (Ast.Object a)       = (put s)^"new "^a
    |add s (Ast.Name x) = x
    |add s (Ast.Method(x,y) ) = ( add 0x)^"."^y
    |add s (Ast.Access(x,y) ) = ( add 0x)^"["^( add 0y)^"]"
    |add s (Ast.FunCall (a, b) )       = (put s)^a^"("^(Parguments  b)^")"
    |add s (Ast.MethodCall(a, b, c))       = (put s)^(add s a)^"."^b^"("^(Parguments  c)^")"
    |add s (Ast.Neg x)   = "( ~"^( add 0x)^")"
    |add s (Ast.Op(a, oper, b)) = "("^( add 0a)^(Ast.binOPtoString oper)^( add 0b)^")"
    |add s (Ast.Closed x) = "(\n" ^ (addlist (s+1)x)^ "\n"^(put s)^")"
    |add s  (Ast.Assign (x, y) ) = (put s) ^( add 0x)^" := "^( add 0y)
    |add s (Ast.OPENIF (a,b) ) =(put s)^blue^"if"^white^" " ^ (add s a) ^blue^" then"^white^(add (s+1)b) 
    |add s (Ast.CLOSEDIF (a,b, c) ) = (add s (Ast.OPENIF(a, b)))^(put s)^blue^"else"^white^(add (s+1)c)
    |add s  (Ast.WHILE (x,y) )   = (put s)^blue^"while"^white^(add s x)^blue^" do"^white^(put (s+1))^(add s y)
    |add s  (Ast.FOR   (a, b, c, d) )= (put s)^blue^"for "^white^a^" := "^( add 0b)^blue^" to "^white^( add 0c)^blue^" do"^white^(add (s+1) d)
    |add s (Ast.BREAK) = (put s)^blue^"break"^white
    |add s (Ast.LET(a, b) ) = (put s)^blue^"let"^white^ "\n"^(adddeclist (s+1)a)^blue^ "\n"^(put s)^"in"^white ^ "\n"^(addlist (s+1)b)^ "\n"^(put s)^blue^"end"^white
   				
and
      adding s (Ast.VariableDec a) = (pvardec s a)^ "\n"
      |adding s (Ast.TypeDec (a, b) ) = (put s)^"type "^a^" = "^(printty 0  b)^ "\n"
      |adding s (Ast.ClassDec (a,b) ) = (put s)^"class "^a^ "{\n"^(addclasslist (s+1)b)^(put s)^ "}\n"
      |adding s (Ast.ClassDecType (a,b,c) ) = (put s)^"class "^a^" extends "^b^(addclasslist (s+1)c)^(put s)^ "\n"
      |adding s (Ast.Import a) = (put s)^"import "^a^ "\n"
      |adding s (Ast.FunctionDec (a,b,c)) = (put s)^"function "^a^"("^(ptyfield b)^") = "^(add s c)^ "\n"
      |adding s (Ast.FunctionDecType (a,b,c,d)) = (put s)^"function "^a^"("^(ptyfield b)^"): "^c^ " = "^(add s d)^ "\n"
      |adding s (Ast.PrimitiveDec (a,b)) = (put s)^"primitive "^a^"("^(ptyfield b)^ "\n"
      |adding s (Ast.PrimitiveDecType (a,b,c)) = (put s)^"primitive "^a^"("^(ptyfield b)^"): "^c^ "\n"
and 

	Parguments [] = ""
	|Parguments (x::[]) = ( add 0 x)^""
	|Parguments (x::xs) = ( add 0 x)^", "^(Parguments xs)


and	ptyfield (Ast.Tyfield a) = let
						fun p [] = " "
						|p ((a,b)::[]) = a^" : "^b
						|p ((a,b)::xs) = a^" : "^b^" , "^(p xs)
					in
						(p a)
					end
and
	printty s (Ast.NameTy a) 		= a
	|printty s (Ast.RecordTy a)  	= "{"^(ptyfield a)^"}"
	|printty s (Ast.ArrayTy a) 	= "array of "^a
	|printty s (Ast.ClassDefCan a)	= "class { \n"^(addclasslist (s+1)a)^(put s)^"}\n"
	|printty s (Ast.ClassDefCanType (a,b)) = "class  extends "^a^"{ \n"^(addclasslist (s+1)b)^(put s)^"}\n"
and
      adddeclist s []      = ""
     |adddeclist s (x::xs) = (adding s x)^ "\n"^(adddeclist s xs)
and 
      addlist s []      = "\n"
     |addlist s (x::[]) = (add s x)
     |addlist s (x::xs) = (add s x)^";"^(addlist s xs)
  and
      classfield s (Ast.MethodDec (a,b,c) ) = (put s)^"method "^a^" ( "^(ptyfield  b)^" ) = "^( add 0c)  
     | classfield s (Ast.MethodDecType (a,b,c,d) ) = (put s)^"method "^a^"( "^(ptyfield b)^" ) : "^c^" = "^( add 0d)
     | classfield s (Ast.ClassAttribute a) = (pvardec s a)    

and
	addclasslist s []      = ""
     |addclasslist s (x::xs) = (classfield s x)^ "\n"^(addclasslist s xs)
and
	pvardec s (Ast.VarDec(a, b)) = (put s)^"var "^a^" := "^(add s b)
	|pvardec s (Ast.VarDecType(a, b,c)) = (put s)^"var "^a^": "^b^" := "^(add s c)
and
  pretty s (Ast.Foo a)  = (add s a)^ "\n"
 |pretty s (Ast.Bar a) = (adddeclist s a)

     
end
