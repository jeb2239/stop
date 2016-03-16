/* Ocamlyacc Parser for Stop */

%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE LSQUARE RSQUARE COMMA COLON
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT CARET MODULO
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token IF ELSE FOR WHILE
%token RETURN VOID 
%token FINAL
%token INCLUDE
%token EOF

/* Primitive Types */

%token INT FLOAT BOOL CHAR UNIT
%token TYPE 

%token DEF CLASS UNIT
%token EOF
%token NEWLINE

/* Primitives */

%token <bool> BOOL_LIT
%token <int> INT_LIT
%token <float> FLOAT_LIT
%token <char> CHAR_LIT
%token <string> STRING_LIT
%token <string> TYPE_ID
%token <string> ID

%token <string> VAR 
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

/* Datatypes */
/* --------- */

primitive:
      INT               { Int }
    | FLOAT             { Float }
    | CHAR              { Char }
    | BOOL              { Bool }
    | UNIT              { Unit }

type_tag:
      primitive { $1 }

datatype:
      type_tag          { Datatype($1) }

/* Statements */
/* ---------- */

stmt_list:
      /* nothing */         { [] }
    | stmt_list stmt        { $2::$1 }

stmt:
      expr SEMI                     { Expr($1) }
    | RETURN SEMI                   { Return(Noexpr) }
    | RETURN expr SEMI              { Return($2) }
    | LBRACE stmt_list RBRACE       { Block(List.rev $2) } 
    | IF LPAREN expr RPAREN stmt %prec NOELSE   
                                        { If($3, $5, Block([Expr(Noexpr)])) }
    | IF LPAREN expr RPAREN stmt ELSE stmt      
                                        { If($3, $5, $7) }
    | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt 
                                        { For($3, $5, $7, $9) }
    | WHILE LPAREN expr RPAREN stmt     
                                        { While($3, $5) }
    | datatype ID SEMI                  { Local($1, $2, Noexpr) }
    | datatype ID ASSIGN expr SEMI      { Local($1, $2, $4) }

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
    | CHAR_LIT          { CharLit($1) }
    | STRING_LIT        { StringLit($1) }
    | ID                { Id($1) }

%%
