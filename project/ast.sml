structure Ast =
struct 
  datatype BinOp = Plus
                        |Minus
                        |Mul
                        |Divide
			|Less
			|Great
			|NotEq
			|Equal
			|LessEq
			|GreatEq
			|And
			|Or

  datatype Expr = Const of int
		 |Closed of Expr list
                 | Op of Expr * BinOp * Expr
                 | Neg of Expr
		 | Assign of Expr*Expr
		 | WHILE of Expr*Expr
		 | FOR of string*Expr*Expr*Expr
		 | OPENIF of (Expr*Expr)
		 | CLOSEDIF of (Expr*Expr*Expr)
		 | BREAK
		 | LET of (Dec list) * Expr list
		 | Method of Expr * string
		 | Access of Expr * Expr
                 | Name of string
		 | NIL
	         | Array of string*Expr*Expr
		 | Record of string * (string*Expr) list
                 | Object   of string
                 | FunCall of string * Expr list
                 | MethodCall of Expr * string * Expr list 
		 | Quote of string
	and Dec = VariableDec of Vdec 
		 |Import of string
		 |PrimitiveDec of string*Tyfields
                 |PrimitiveDecType of string*Tyfields*string
		 |FunctionDec of string*Tyfields*Expr
                 |FunctionDecType of string*Tyfields*string*Expr
		 |TypeDec of string*Ty
                 |ClassDec of string*Classfield list
                 |ClassDecType of string*string*Classfield list
	and Tyfields = Tyfield of (string*string) list
	and Ty 	= NameTy of string
		 |RecordTy of Tyfields
		 |ArrayTy of string
		 |ClassDefCan of Classfield list
		 |ClassDefCanType of string*Classfield list
        and Classfield = ClassAttribute of Vdec
			|MethodDec of string*Tyfields*Expr
                        |MethodDecType of string*Tyfields*string*Expr 
        and Vdec =  VarDec of string * Expr
                 |VarDecType of string * string*Expr 
  datatype Program = Foo of Expr
                    |Bar of Dec list

    fun binOPtoString x = case x of Mul => "*"
				 |  Divide  => "/"
				 |  Plus  => "+"
				 |  Minus  => "-"
				 |  Less  => "<"
				 |  Great  => ">"
				 |  NotEq  => "<>"
				 |  Equal  => "="
				 |  LessEq  => "<="
				 |  GreatEq  => ">="
				 |  And  => "&"
				 |  Or  => "|"
				 
    fun plus  a b = Op (a, Plus, b)
    fun minus a b = Op (a, Minus, b)
    fun mul   a b = Op (a, Mul, b)
    fun divide a b = Op(a, Divide, b)
    fun less a b = Op(a, Less, b)
    fun great a b = Op(a, Great, b)
    fun equal a b = Op(a, Equal, b)
    fun notequal a b = Op(a, NotEq, b)
    fun lessequal a b = Op(a, LessEq, b)
    fun greatequal a b = Op(a, GreatEq, b)
    fun or a b = Op(a, Or, b)
    fun logicaland a b = Op(a, And, b)
    fun assign a b = Assign(a, b)
    fun While a b = WHILE(a, b)
    fun For a b c d = FOR (a, b, c, d)
    fun Openif a b = OPENIF(a, b)
    fun Closedif a b c = CLOSEDIF (a, b, c)
end
