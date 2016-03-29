%{ open Ast %}



%token SEMI LPAREN RPAREN LBRACE RBRACE LSQUARE RSQUARE COMMA COLON
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT CARET MODULO DOT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token IF ELSE ELSEIF FOR WHILE
%token RETURN VOID SPEC PUB PRIV
%token FINAL VAR ANON PATTERN FUN 
%token INCLUDE
%token EOF
%token FUNCTION ARROW


/* Primitive Types */

%token INT FLOAT BOOL CHAR
%token TYPE THIS
%token UNIT

%token DEF CLASS UNIT 
%token EOF
%token NEWLINE

/* Primitives */

%token <int> INT_LIT
%token <float> FLOAT_LIT
%token <bool> BOOL_LIT
%token <string> STRING_LIT
%token <string> TYPE_ID
%token <string> ID
%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEG

%start expr
%type <Ast.expr> expr


%%

/*program:
	stmt_list EOF {Program($1)}*/


literals:
      INT_LIT           { IntLit($1) }
    | FLOAT_LIT         { FloatLit($1) }
    | TRUE              { BoolLit(true) }
    | FALSE             { BoolLit(false) }
    | ID                { Id($1) }
  /*  | fun_lit 			{ $1 }*/
/*
fun_lit:
	LPAREN formal_list RPAREN COLON dtype stmts {FuncLit($2,$5,$6)}
*/

/*
formals_opt:
     nothing { [] }
  | formal_list   { List.rev $1 }

formal_list:
    ID COLON dtype                   { [($3,$1)] }
  | formal_list COMMA ID COLON dtype  { ($5,$3) :: $1 }

types_opt:
	nothing { [] }
	| type_list {List.rev $1}

type_list:
	dtype {[$1]}
	| type_list COMMA dtype {$1::$3}
*/
	

dtype:
	INT {Int_t}
	| BOOL {Bool_t}
	| FLOAT {Float_t}
	| CHAR {Char_t}
	| UNIT {Unit_t}
	/*| TYPE_ID   {Name_t($1)}*/
/*	| fun_type { $1 }*/
	
/*
fun_type:
	FUN LPAREN type_opt RPAREN ARROW dtype { Functiontype($3,$6) }
*/



vdecl:
  VAR ID COLON dtype { Vdec($2,$4) }



expr:
    literals          { $1 }
  | expr PLUS   expr { Binop($1, Add,   $3); }
  | expr MINUS  expr { Binop($1, Sub,   $3); }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or,    $3) }
  | MINUS expr %prec NEG { Unop(Neg, $2) }
  | NOT expr         { Unop(Not, $2) }
  | vdecl             { $1 }
 /* | ID ASSIGN expr   { Assign($1, $3) }*/
 /* | ID LPAREN actuals_opt RPAREN { Call($1, $3) }*/ /*if we call a named function*/
  /*| fun_lit            {$1} */
 /* | LPAREN expr RPAREN { $2 }*/
/*
stmt:
    expr SEMI { Expr $1 }
  | RETURN SEMI { Return Noexpr }
  | RETURN expr SEMI { Return $2 }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }

actuals_opt:
     nothing  { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
*/