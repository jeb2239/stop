/* Ocamlyacc Parser for Stop */

%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE LSQUARE RSQUARE COMMA COLON
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT CARET MODULO
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token IF ELSE ELSEIF FOR WHILE
%token RETURN VOID 
%token FINAL
%token EOF

/* Primitive Types */

%token INT FLOAT BOOL
%token TYPE 

%token DEF CLASS UNIT
%token EOF
%token NEWLINE

/* Primitives */

%token <int> INT_LIT
%token <float> FLOAT_LIT
%token <bool> BOOL_LIT
%token <string> TYPE_ID
%token <string> ID

%token <string> VAR 
%token <float->float> FNCT

%right EQ
%left PLUS MINUS
%left TIMES DIVIDE
%left NEG
%right CARET 

%start expr
%type <Ast.expr> expr

%%

/* Context-Free Grammar */

expr:
      literals              { $1 }
    | expr PLUS     expr    { Binop($1, Add, $3) }
    | expr MINUS    expr    { Binop($1, Sub, $3) }
    | expr TIMES    expr    { Binop($1, Mult, $3) }
    | expr DIVIDE   expr    { Binop($1, Div, $3) }
    /*
    | expr EQ       expr    { Binop($1, Equal, $3) }
    | expr NEQ      expr    { Binop($1, Neq, $3) }
    | expr LT       expr    { Binop($1, Less, $3) }
    | expr LEQ      expr    { Binop($1, Leq, $3) }
    | expr GT       expr    { Binop($1, Greater, $3) }
    | expr GEQ      expr    { Binop($1, Geq, $3) }
    | expr AND      expr    { Binop($1, And, $3) }
    | expr OR       expr    { Binop($1, Or, $3) }
    */

literals:
      INT_LIT           { IntLit($1) }
    | FLOAT_LIT         { FloatLit($1) }
    | TRUE              { BoolLit(true) }
    | FALSE             { BoolLit(false) }
    | ID                { Id($1) }

/*
line: NEWLINE { }
	| exp NEWLINE { printf "\t%.10g\n" $1; flush stdout}
	;

exp: 
	| VAR {try Hashtbl.find var_table $1
		with Not_found -> printf "no such variable '%s'\n" $1;
		0.0

	}
	| VAR EQ exp {Hashtbl.replace var_table $1 $3;
		$3
	}

	| FNCT LPAREN exp RPAREN        { $1 $3 }
	| exp PLUS exp                  { $1 +. $3 }
	| exp MINUS exp                 { $1 -. $3 }
	| exp TIMES exp                 { $1 *. $3 }
	| exp DIVIDE exp                { $1 /. $3 }
	| MINUS exp %prec NEG           { -. $2 }
	| exp CARET exp                 { $1 ** $3 }
	| LPAREN exp RPAREN             { $2 }
	;
*/

%%
