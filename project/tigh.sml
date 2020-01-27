structure Tigh =
struct
val interactive = tiglex.makeLexer (fn _ => TextIO.inputN (TextIO.stdIn,1))


fun lexfile file = let val strm = TextIO.openIn file
		   in tiglex.makeLexer (fn n => TextIO.inputN(strm,n))
		   end



fun whitespace 0 = ""
    |whitespace n = " "^(whitespace (n-1));

fun tokentostring token = case token of
                            (COMMENT x) => x
			|(QUOTES x)=>x
                           | (INVI NEWLINE)=>"\n"
			  |  (WHITE (x,WHITESPACE)) => whitespace x
			|(INVI EOF)=>""
			|(CONST x)=>Int.toString x
			|(TEXT x) => x
			|(KEY LET)=>"let"
			|(KEY NIL)=>"nil"
			|(KEY WHILE)=>"while"
			|(KEY FOR)=>"for"
			|(KEY TO)=>"to"
			|(KEY BREAK)=>"break"
			|(KEY END)=>"end"
			|(KEY FUNCTION)=>"function"
			| (KEY ARRAY)=>"array"
			| (KEY IF)=>"if"
			| (KEY THEN)=>"then"
			| (KEY ELSE)=>"else"
			| (KEY DO)=>"do"
			| (KEY IN)=>"in"
			| (KEY OF)=>"of"
			| (KEY VAR)=>"var"
			| (KEY TYPE)=>"type"
			| (KEY IMPORT)=>"import"
			| (KEY PRIMITIVE)=>"primitive"
			| (OP PLUS)=>"+"
			| (OP MINUS)=>"-"
			| (OP MUL)=>"*"
			| (OP DIV)=>"/"
			| (OP AND)=>"&"
			| (OP OR)=>"|"
			| (OP EQUAL)=>"="
			| (OP NOTEQUAL)=>"<>"
			| (OP ASS)=>":="
			| (OP LESS)=>"<"
			| (OP GRT)=>">"
			| (OP LEQ)=>"<="
			| (OP GEQ)=>">="
			| (SYM COMMA)=>","
			| (SYM COLON)=>":"
			| (SYM SEMICOLON)=>";"
			| (SYM DOT)=>"."
			| (SYM LPAREN)=>"("
			| (SYM RPAREN)=>")"
			| (SYM LCURBR)=>"{"
			| (SYM RCURBR)=>"}"
			| (SYM LSQBR)=>"["
			| (SYM RSQBR)=>"]"
			| (OBJ CLASS)=>"class"
			| (OBJ EXTENDS)=>"extends"
			| (OBJ METHOD)=>"method"
			| (OBJ NEW)=>"new"

			
fun prettyprint token  = case token of
                         (COMMENT x) => print("\027[30m"^(tokentostring token))
			|(QUOTES x) => print("\027[96m"^(tokentostring token)) 
			|(INVI _)    => print(tokentostring token)
			|(WHITE _)    => print(tokentostring token)
			|(CONST x)   => print("\027[94m"^(tokentostring token))
			|(TEXT x) => print("\027[32m"^(tokentostring token))
 			|(KEY _) => print("\027[33m"^(tokentostring token))
			|(OBJ _) =>print("\027[34m"^(tokentostring token))
			|(SYM _) =>print("\027[37m"^(tokentostring token))
			|(OP _) =>print("\027[35m"^(tokentostring token))

fun runWithLexer lexer = let fun loop () = case lexer () of
					         (token) => if token=(INVI EOF) then print("\027[0m") else loop (prettyprint (token))
			 in loop ()
			 end
val _ =  ( case CommandLine.arguments() of
	       [] => runWithLexer interactive
	    |  xs => (List.map (runWithLexer o lexfile) xs; ())
	 )
end
