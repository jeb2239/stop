%{
	open Printf
	open Lexing

	let var_table = Hashtbl.create 16

%}

%token NEWLINE
%token LPAREN RPAREN EQ
%token <float> NUM
%token PLUS MINUS MULT DIV CARET
%token <string> VAR 
%token <float->float> FNCT

%right EQ
%left PLUS MINUS
%left MULT DIV 
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

	|FNCT LPAREN exp RPAREN { $1 $3 }
	|exp PLUS exp {$1 +. $3}
	|exp MINUS exp {$1 -. $3}
	|exp MULT exp {$1 *. $3}
	|exp DIV exp {$1 /. $3}
	|MINUS exp %prec NEG { -. $2}
	|exp CARET exp { $1 ** $3}
	|LPAREN exp RPAREN {$2}
	;
%%