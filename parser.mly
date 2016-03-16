/* Ocamlyacc Parser for Stop */

%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE LSQUARE RSQUARE COMMA COLON
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT CARET MODULO
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token IF ELSE ELSEIF FOR WHILE
%token RETURN VOID SPEC PUB PRIV
%token FINAL VAR ANON PATTERN FUN
%token INCLUDE
%token EOF

/* Primitive Types */

%token INT FLOAT BOOL
%token TYPE 
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

//%token <string> VAR 
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
      includes spec_decls cdecls EOF          { Program($1, $2 , $3) }

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

/* Specs*/
/*------*/
spec_decls:
	spec_decls_list  {List.rev $1}

spec_decls_list:
	
	|spec_decl					{[$1]}
	|spec_decls_list spec_decl {$2::$1}

spec_decl:
	| SPEC ID LBRACE sbody RBRACE SEMI


sbody:
		{{
		methods=[];
		typereq=[];
		}}
	| sbody typereq 
	{{
		methods=$1.methods;
		typereq=$2::$1.typereq;
		
	}}

	| sbody method_decl 
	{
		{
			methods=$2::$1.methods;
			typereq=$1.typereq;
		}
	}


/*Class decl*/

class_decls:
	class_decl_list { List.rev   }

class_decl_list:
	class_decl {[$1]}
	| cdecl_list cdecl {$2::$1}

class_decl:
	 CLASS ID LBRACE cbody RBRACE {{

	 	}}
	



cbody:
	{{
		fields = [];
		pattern_constructors = [];
		methods =[];
		}}
	| cbody field {{
			fields = $2 :: $1.fields;
			pattern_constructors = $1.pattern_constructors;
			methods = $1.methods;
		}}
	| cbody patten_constructor {{
		fields = $1.fields;
		pattern_constructors = $2::$1.pattern_constructors;
		methods = $1.methods;

		}}
	| cbody method_decl {{

		fields = $1.fields;
		pattern_constructors = $1.pattern_constructors;
		methods = $2::$1.methods;

		}}


/***********
Constructor
********/

pattern_constructor:
	PATTERN ASSIGN LPAREN formal_option RPAREN LBRACE stmt_list RBRACE {
		{
			visibility=Pub;
			name="pattern"
			returnType
			formal_param = $4;
			body=List.rev $7;
			(*if we were some how going to do inheritance we would have 
				to add shit here*)
		}
	}


	





/******************
 FIELDS
******************/
visibility:
	  PRIV   {Priv}
	| PUB	 {Pub}
/* Statements */
/* ---------- */

field:
	visibility ID COLON dtype SEMI {Field($1,$4,$2)}

/************
METHODS + functions
********/
method_decl:
	visibility DEF ID ASSIGN LPAREN formal_option RPAREN COLON dtype LBRACE stmt_list RBRACE
	{
		{
			visibility=$1;
			name = $3;
			returnType = $9;
			formal_param = $6;
			body = $11;
		}
	}

function_decl:
	| FUN ID ASSIGN LPAREN formal_option RPAREN COLON dtype LBRACE stmt_list RBRACE
	{{
		name=$2;
		returnType=$8;
		formal_param=$5;
		body=$10;
		}}
	| ANON LPAREN formal_option RPAREN COLON dtype LBRACE stmt_list RBRACE
	{{
		name= (*we need to make a unique name for every anon function*)
		returnType=$8;
		formal_param=$5;
		body=$10;
		}}

formals_option:
		/* nothing */ { [] }
	| 	formal_list   { List.rev $1 }

formal_list:
		formal                   { [$1] }
	| 	formal_list COMMA formal_param { $3 :: $1 }

formal_param:
	 ID COLON dtype { Formal($2, $1) }

actuals_opt:
		/* nothing */ { [] }
	| 	actuals_list  { List.rev $1 }

actuals_list:
		expr                    { [$1] }
	| 	actuals_list COMMA expr { $3 :: $1 }

/******
datatypes
******/
primitive:
		INT 		{ Int_t }
	| 	FLOAT		{ Float_t } 
	| 	CHAR		{ Char_t }
	| 	BOOL 		{ Bool_t }
	| 	UNIT    	{ Unit_t }

name:
	CLASS ID { Class_t($2)}
spec:
	SPEC ID {Spec_t($2)}
type_tag:
	primitive {$1}
	|name {$1}

array_type:
	type_tag LBRACKET brackets RBRACKET { Arraytype($1, $3) }
 

concrete_type_tag:
	type_tag  {$1}
	| array_type {$1} 

abstract_type_tag:
	|spec {$1}
	|concrete_type_tag {$1}

dtype:
		

brackets:
		/* nothing */ 			   { 1 }
	| 	brackets RBRACKET LBRACKET { $1 + 1 }


stmt_list:
      /* nothing */         { [] }
    | stmt_list stmt        { $2::$1 }

// NOTE: Had to differ from spec here becuase no SEMI causes ambiguity with MINUS:
// We have rules for expr MINUS expr, MINUS expr, so there's always a shift/reduce conflict

stmt:
      expr SEMI                     { Expr($1) }
    | RETURN SEMI                   { Return(Noexpr) }
    | RETURN expr SEMI              { Return($2) }
    | LBRACE stmt_list RBRACE       { Block(List.rev $2) } 
    | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([Expr(Noexpr)])) }
    | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
    | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt { For($3, $5, $7, $9) }
    | WHILE LPAREN expr RPAREN stmt { While($3, $5) }


/* Functions */
/* --------- */

/*
fdecls:
                            { [] }
    | fdecl_list            { List.rev $1 }

fdecl_list:
      fdecl                 { [$1] }
    | fdecl_list fdecl      { $2::$1 }

fdecl:
    DEF ID                  { $2 }
*/

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
