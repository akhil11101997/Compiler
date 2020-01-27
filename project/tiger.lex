fun eof   ()      =  (INVI EOF);

fun charsToInt m (x :: xs) = charsToInt (10 * m + ord x - ord #"0") xs
  | charsToInt m []        = m

fun toSigned (#"-" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"~" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"+" :: xs) =   charsToInt 0 xs
  | toSigned xs           =   charsToInt 0 xs
type lexresult     = Token;
val toInt        = toSigned o String.explode


%%
%structure tiglex
ws    = [\ \t];
digit = [0-9]+;
str   = [a-z_A-Z]+;
%%
"/*"([^*]|\*+[^*/])*\*+"/"       => ( (COMMENT yytext));
"\"".*"\""         => ((QUOTES yytext));
\n({ws}*\n)*       => ( (INVI NEWLINE));
{ws}+         => (  (WHITE (size yytext,WHITESPACE)));
{digit}+      => ( (CONST (toInt yytext)));

"let"           => ( (KEY LET));
"var"           => ( (KEY VAR));
"nil"           => ( (KEY NIL));
"break"           => ( (KEY BREAK));
"end"           => ( (KEY END));
"array"           =>( (KEY ARRAY));
"if"           =>( (KEY IF));
"then"           =>( (KEY THEN));
"else"           =>( (KEY ELSE));
"do"           =>( (KEY DO));
"in"           =>( (KEY IN));
"of"           =>( (KEY OF));
"var"           =>( (KEY VAR));
"function"       =>(KEY FUNCTION);
"type"           =>( (KEY TYPE));
"import"           =>( (KEY IMPORT));
"primitive"           =>( (KEY PRIMITIVE));
"+"		=>( (OP PLUS));
"-"		=>( (OP MINUS));
"*"		=>( (OP MUL));
"/"		=>( (OP DIV));
"&"		=>( (OP AND));
"|"		=>( (OP OR));
"="		=>( (OP EQUAL));
"<>"		=>( (OP NOTEQUAL));
":="		=>( (OP ASS));
"<"		=>( (OP LESS));
">"		=>( (OP GRT));
"<="		=>( (OP LEQ));
">="		=>( (OP GEQ));
","		=>( (SYM COMMA));
":"		=>( (SYM COLON));
";"		=>( (SYM SEMICOLON));
"."		=>( (SYM DOT));
"("		=>( (SYM LPAREN));
")"		=>( (SYM RPAREN));
"{"		=>( (SYM LCURBR));
"}"		=>( (SYM RCURBR));
"["		=>( (SYM LSQBR));
"]"		=>( (SYM RSQBR));
"class"		=>( (OBJ CLASS));
"extends"	=>( (OBJ EXTENDS));
"method"	=>( (OBJ METHOD));
"new"		=>( (OBJ NEW));
{str}          => ( (TEXT yytext));

