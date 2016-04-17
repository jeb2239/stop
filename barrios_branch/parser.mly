%{ open Ast
open Core.Std %}



%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA COLON
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT CARET MODULO DOT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token IF ELSE ELSEIF FOR WHILE
%token RETURN VOID SPEC PUB PRIV
%token FINAL VAR ANON PATTERN FUN METHOD
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

%start program
%type <Ast.program> program


%%

program:
	stmt_list cdecls EOF {Program($1,$2)}


cdecls:
    cdecl_list    { List.rev $1 }

cdecl_list:
    cdecl             { [$1] }
  | cdecl_list cdecl  { $2::$1 }

cdecl:
    CLASS TYPE_ID LBRACE cbody RBRACE { {
      cname = $2;
      cbody = $4;
    } }
  

cbody:
    /* nothing */ { { 
      fields = [];
      constructors = [];
      methods = [];
    } }
  |   cbody field { { 
      fields = $2 :: $1.fields;
      constructors = $1.constructors;
      methods = $1.methods;
    } }
  |   cbody constructor { { 
      fields = $1.fields;
      constructors = $2 :: $1.constructors;
      methods = $1.methods;
    } }
  |   cbody method_dec { { 
      fields = $1.fields;
      constructors = $1.constructors;
      methods = $2 :: $1.methods;
    } }

field:
  VAR ID COLON datatype SEMI {Field($2,$4)}

method_dec:
  METHOD ID ASSIGN LPAREN formal_opt RPAREN COLON datatype LBRACE stmt_list RBRACE SEMI { Method($2,$5,$8,$10)}

constructor:
  PATTERN ASSIGN LPAREN formal_opt RPAREN COLON datatype LBRACE stmt_list RBRACE SEMI {Constructor($4,$7,$9)}


literals:
      INT_LIT           { IntLit($1) }
    | FLOAT_LIT         { FloatLit($1) }
    | TRUE              { BoolLit(true) }
    | FALSE             { BoolLit(false) }
    | ID                { Id($1) }
    | fun_lit            {$1}
  /*  | fun_lit 			{ $1 }*/
/*
fun_lit:
	ANON fun_body

*/

fun_lit:
  LPAREN formal_opt RPAREN COLON datatype LBRACE stmt_list RBRACE {FuncLit($2,$5,$7)}


formal_opt:
      { [] }
  | formal_list   { List.rev $1 }

formal_list:
    ID COLON datatype                   { [Formal($1,$3)] }
  | formal_list COMMA ID COLON datatype  { Formal($3,$5) :: $1 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }

types_opt:
	 { [] }
	| type_list {List.rev $1}

type_list:
	 datatype {[$1]}
	| type_list COMMA datatype {$3::$1}

	
datatype:
    type_tag   { $1 }
   | array_type { $1 }


type_tag:
    primitive       { Datatype($1)}
  | object_type     { $1 }
  | fun_type        { $1 }

primitive:
    INT             { Int_t }
  | FLOAT           { Float_t }
  | CHAR            { Char_t }
  | BOOL            { Bool_t }
  | UNIT            { Unit_t }
 
object_type:
    TYPE_ID { Objecttype($1) }
array_type:
    type_tag LBRACKET brackets RBRACKET { Arraytype($1, $3) }

brackets:
               { 1 }
  | brackets RBRACKET LBRACKET { $1 + 1 }

bracket_args:
    LBRACKET expr            { [$2] }
  |   bracket_args RBRACKET LBRACKET expr { $4 :: $1 }

/*
	INT {Int_t}
	| BOOL {Bool_t}
	| FLOAT {Float_t}
	| CHAR {Char_t}
	| UNIT {Unit_t}
  | FUN LPAREN types_opt RPAREN ARROW dtype {Functiontype($3,$6)}*/
	/*| TYPE_ID   {Name_t($1)}*/

	


fun_type:
	FUN LPAREN types_opt RPAREN ARROW datatype { Functiontype($3,$6) }



/*
vdecl:
  VAR ID COLON dtype { Vdec($2,$4) }
*/
expr_opt:
    { Noexpr }
  | expr          { $1 }


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
  | LPAREN expr RPAREN            { $2 }
  | TYPE_ID LPAREN actuals_opt RPAREN {Objectcreate($1,$3)}
  | type_tag bracket_args RBRACKET {Arraycreate($1,List.rev $2)}
 /* | vdecl             { $1 }*/
  | expr bracket_args RBRACKET      { Arrayaccess($1, List.rev $2) }
  | expr ASSIGN expr   { Assign($1, $3) }


 /* |
  | ID ASSIGN expr {Assign($1,$3)}*/


 /* | ID LPAREN actuals_opt RPAREN { Call($1, $3) }*/ /*if we call a named function*/
  /*| fun_lit            {$1} */
 /* | LPAREN expr RPAREN { $2 }*/
stmt_list:
       { [] }
  | stmt_list stmt { $2 :: $1 }
  
stmt:
    expr SEMI { Expr($1) }
  | RETURN SEMI { Return Noexpr }
  | RETURN expr SEMI { Return $2 }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
  | VAR ID COLON datatype SEMI {Static_init($2,$4,Noexpr)}
  | VAR ID COLON datatype ASSIGN expr SEMI { Static_init($2,$4,$6) }
  | FUNCTION ID ASSIGN fun_lit SEMI {Static_init($2,(type_of_lit $4),$4)}

  


/*
actuals_opt:
     nothing  { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
*/