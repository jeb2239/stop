/* Ocamlyacc Parser for Stop */

%{ open Ast
   open Core.Std %}

%token SEMI LPAREN RPAREN LBRACE RBRACE LSQUARE RSQUARE COMMA COLON
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT CARET MODULO
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token IF ELSE ELSEIF FOR WHILE
%token RETURN VOID 
%token FINAL
%token INCLUDE
%token EOF
 
/* Primitive Types */

/*%token INT FLOAT BOOL*/
%token TYPE 
/*%token UNIT*/

%token DEF CLASS UNIT VAR
/*%token EOF*/
%token NEWLINE

/* Primitives */

%token <int> INT_LIT
%token <float> FLOAT_LIT
%token <bool> BOOL_LIT
%token <string> STRING_LIT
%token <string> TYPE_ID
%token <string> ID

/*%token <string> VAR */
%token <float->float> FNCT

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

%start program
%type <Ast.program> program

%%

/* Context-Free Grammar */
/* -------------------- */

program:
      includes stmt_list EOF          { Program($1, $2) }

/* Includes */
/* -------- */

includes: 
      /* nothing */         { [] }
    | include_list          { List.rev $1 }

include_list:
      include_decl                  { [$1] }
    | include_list include_decl     { $2::$1 }

include_decl:
    INCLUDE STRING_LIT      { Include($2) }


cdecls:
    cdecl_list    { List.rev $1 }

cdecl_list:
    cdecl             { [$1] }
  | cdecl_list cdecl  { $2::$1 }

cdecl:
    CLASS TYPE_ID ASSIGN LPAREN match_list RPAREN LBRACE cbody RBRACE { {
      cname = $2;
      extends = NoParent;
      cbody = $8
    } }

cbody:
    /* nothing */ { { 
      fields = [];
      methods = [];
    } }
  |   cbody field { { 
      fields = $2 :: $1.fields;   
      methods = $1.methods;
    } }
  |   cbody fdecl { { 
      fields = $1.fields;
      methods = $2 :: $1.methods;
    } }

field:
    VAR ID COLON datatype SEMI { Field($1, $2, $3) }

/******
methods
*****/

fname:
  ID { $1 }

fdecl:
  DEF fname ASSIGN LPAREN formals_opt RPAREN COLON TYPE_ID LBRACE stmt_list RBRACE 
  { 
    {
      
      fname = FName($2);
      returnType = $7;
      formals = $4;
      body = List.rev $10;
      overrides = false;
      root_cname = None;
    } 
  }
/*Lists*/

formals_opt:
    /* nothing */ { [] }
  |   formal_list   { List.rev $1 }

formal_list:
    formal                   { [$1] }
  |   formal_list COMMA formal { $3 :: $1 }

formal:
  VAR ID COLON datatype { Formal($2 , $1) }





/* Statements */
/* ---------- */

stmt_list:
      /* nothing */         { [] }
    | stmt_list stmt        { $2::$1 }

// NOTE: Had to differ from spec here becuase no SEMI causes ambiguity with MINUS:
// We have rules for expr MINUS expr, MINUS expr, so there's always a shift/reduce conflict




stmt:
      expr SEMI                      { Expr($1) }
    | RETURN SEMI                   { Return(Noexpr) }
    | RETURN expr SEMI              { Return($2) }
    | LBRACE stmt_list RBRACE       { Block(List.rev $2) } 
    | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([Expr(Noexpr)])) }
    | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
    | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt { For($3, $5, $7, $9) }
    | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
    










expr_opt:
      /* nothing */         { Noexpr }
    | expr                  { $1 }

expr:
      literals              { $1 }
    | expr PLUS     expr    { Binop($1, Add, $3) }
    | expr MINUS    expr    { Binop($1, Sub, $3) }
    | expr TIMES    expr    { Binop($1, Mult, $3) }
    | expr DIVIDE   expr    { Binop($1, Div, $3) }
    | expr EQ       expr    { Binop($1, Equal, $3) }
    | expr NEQ      expr    { Binop($1, Neq, $3) }
    | expr LT       expr    { Binop($1, Less, $3) }
    | expr LEQ      expr    { Binop($1, Leq, $3) }
    | expr GT       expr    { Binop($1, Greater, $3) }
    | expr GEQ      expr    { Binop($1, Geq, $3) }
    | expr AND      expr    { Binop($1, And, $3) }
    | expr OR       expr    { Binop($1, Or, $3) }   
    | MINUS expr %prec NEG  { Unop(Neg, $2) } 
    | NOT expr              { Unop(Not, $2) }
    | LPAREN expr RPAREN    { $2 }

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
