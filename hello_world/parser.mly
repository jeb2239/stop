%{ open Ast %}



%token SEMI LPAREN RPAREN LBRACE RBRACE LSQUARE RSQUARE COMMA COLON
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT CARET MODULO DOT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token IF ELSE ELSEIF FOR WHILE
%token RETURN VOID SPEC PUB PRIV
%token FINAL VAR ANON PATTERN FUN 
%token INCLUDE
%token EOF
%token FUNCTION


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

%start program
%type <Ast.program> program


%%




literals:
      INT_LIT           { IntLit($1) }
    | FLOAT_LIT         { FloatLit($1) }
    | TRUE              { BoolLit(true) }
    | FALSE             { BoolLit(false) }
    | ID                { Id($1) }
    | fun_lit 			{ }

fun_lit:
	LPAREN formal_list RPAREN COLON dtype stmts {FuncLit($2,$5,$6)}



formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    ID COLON dtype                   { [($1,$2)] }
  | formal_list COMMA ID COLON dtype  { ($3,$4) :: $1 }

types_opt:
	/*nothing*/ { [] }
	| type_list {List.rev $1}

type_list:
	dtype {[$1]}
	| type_list COMMA dtype {$1::$3}

	

dtype:
	INT {Int_t}
	| BOOL {Bool_t}
	| FLOAT {Float_t}
	| CHAR {Char_t}
	| UNIT {Unit_t}
	| ID   {Name_t($1)}
	| fun_type { $1 }
	

fun_type:
	FUN LPAREN type_opt RPAREN COLON dtype { Functiontype($3,$6) }



