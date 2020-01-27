datatype objkeywords=CLASS
    |EXTENDS
    |METHOD
        |NEW
datatype symbols =  COMMA
        |COLON
    |SEMICOLON
    |LPAREN
    |RPAREN
    |LSQBR
    |RSQBR
    |LCURBR
    |RCURBR
    |DOT
datatype operators=PLUS
    |MINUS
    |MUL
    |DIV
    |EQUAL
    |NOTEQUAL
    |LESS
    |LEQ
    |GRT
    |GEQ
    |AND
    |OR
    |ASS
datatype keywords =  ARRAY
    |IF
    |THEN
    |ELSE
    |WHILE
    |FOR
    |TO
    |DO
    |LET
    |IN
    |END
    |OF
    |BREAK
    |NIL
    |FUNCTION
    |VAR
    |TYPE
    |IMPORT
    |PRIMITIVE
datatype invisible = NEWLINE
    |EOF
datatype white = WHITESPACE
datatype Token=
     CONST of int
    |TEXT of string
    |COMMENT of string
    |QUOTES of string
    |OBJ of objkeywords
    |SYM of symbols
    |OP of operators
    |KEY of keywords
    | INVI of invisible
    |WHITE of (int*white)
