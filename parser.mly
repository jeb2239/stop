/* Ocamlyacc Parser for Stop */

%{
	open Printf
	open Lexing
	let var_table = Hashtbl.create 16
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA COLON
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT CARET
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF ELSE FOR WHILE INT BOOL VOID
%token EOF

%token NEWLINE
%token <float> NUM
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

input: /*none*/ { }
	| input line { }
	;

line: NEWLINE { }
	| exp NEWLINE { printf "\t%.10g\n" $1; flush stdout}
	;

exp: NUM	{$1}
	| VAR {try Hashtbl.find var_table $1
		with Not_found -> printf "no such variable '%s'\n" $1;
		0.0

	}
	|VAR EQ exp {Hashtbl.replace var_table $1 $3;
		$3
	}

	| FNCT LPAREN exp RPAREN { $1 $3 }
	| exp PLUS exp { $1 +. $3 }
	| exp MINUS exp { $1 -. $3 }
	| exp TIMES exp { $1 *. $3 }
	| exp DIVIDE exp { $1 /. $3 }
	| MINUS exp %prec NEG { -. $2 }
	|exp CARET exp { $1 ** $3 }
	|LPAREN exp RPAREN { $2 }
	;
%%
