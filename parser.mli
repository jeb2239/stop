type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LSQUARE
  | RSQUARE
  | COMMA
  | COLON
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | NOT
  | CARET
  | MODULO
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | TRUE
  | FALSE
  | AND
  | OR
  | IF
  | ELSE
  | ELSEIF
  | FOR
  | WHILE
  | RETURN
  | VOID
  | FINAL
  | INCLUDE
  | EOF
  | INT
  | FLOAT
  | BOOL
  | TYPE
  | UNIT
  | DEF
  | CLASS
  | VAR
  | NEWLINE
  | INT_LIT of (int)
  | FLOAT_LIT of (float)
  | BOOL_LIT of (bool)
  | STRING_LIT of (string)
  | TYPE_ID of (string)
  | ID of (string)
  | FNCT of (float->float)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
