/* Ocamlyacc Parser for Stop */

%{
	open Printf
	open Lexing
	let var_table = Hashtbl.create 16
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LSQUARE RSQUARE COMMA COLON
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT CARET MODULO
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token IF ELSE ELSEIF FOR WHILE
%token RETURN VOID 
%token FINAL

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

%token <string> VAR 
%token <float->float> FNCT

%right EQ
%left PLUS MINUS
%left TIMES DIVIDE
%left NEG
%right CARET 

%start input 
%type <unit> input 

%%

/* Context-Free Grammar */

input: { } 

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

%%
