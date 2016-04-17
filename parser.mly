/* Ocamlyacc Parser for Stop */

%{ 
    open Ast
    open Core.Std 
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA COLON
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT CARET MODULO
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token IF ELSE FOR WHILE
%token RETURN VOID 
%token FINAL
%token INCLUDE
%token DOT
%token FUNCTION CLASS METHOD
%token ARROW FATARROW
%token PUB PRIV ANON
%token PATTERN

/* Primitive Types */

%token INT FLOAT BOOL CHAR FUN UNIT
%token TYPE VAR THIS
%token DEF EXTENDS 
%token EOF

/* Primitives */

%token <bool> BOOL_LIT
%token <int> INT_LIT
%token <float> FLOAT_LIT
%token <char> CHAR_LIT
%token <string> STRING_LIT

%token <string> ID
%token <string> TYPE_ID

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left AND OR
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%right NOT NEG
%right RBRACKET
%left LBRACKET
%right DOT

%start program
%type <Ast.program> program

%%

/* Context-Free Grammar */
/* -------------------- */

program:
      includes fdecls EOF          { Program($1, $2) }

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

datatype:
    type_tag        { Datatype($1) }
  | array_type      { $1 }
  | object_type     { $1 }

type_tag:
    primitive       { $1 }

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
    /* nothing */              { 1 }
  | brackets RBRACKET LBRACKET { $1 + 1 }

/* Functions */
/* --------- */

fdecls:
    fdecl_list          { List.rev $1 }

fdecl_list:
    fdecl               { [$1] }
  | fdecl_list fdecl    { $2::$1 }

fdecl:
   FUNCTION ID ASSIGN LPAREN formals_opt RPAREN COLON datatype LBRACE stmts RBRACE { { 
            fname = FName($2);
            return_t = $8;
            formals = $5;
            body = $10;
    } }

/* Formals and Actuals */
/* ------------------- */

formals_opt:
    /* nothing */               { [] }
  | formal_list                 { List.rev $1 }

formal_list:
    formal                      { [$1] }
  | formal_list COMMA formal    { $3::$1 }

formal:
    ID COLON datatype           { Formal($3, $1) }

actuals_opt:
    /* nothing */               { [] }
  | actuals_list                { List.rev $1 }

actuals_list:
    expr                        { [$1] }
  | actuals_list COMMA expr     { $3::$1 }

/* Classes */
/* ------- */

//cdecls:
//      /* nothing */         { [] }
//    | cdecl_list            { List.rev $1 }
//
//cdecl_list:
//      cdecl                 { [$1] }
//    | cdecl_list cdecl      { $2::$1 }
//
//cdecl:
//      CLASS ID LBRACE cbody RBRACE { {
//            cname = $2;
//            extends = NoParent;
//            cbody = $4;
//      } }
//    | CLASS ID EXTENDS ID LBRACE cbody RBRACE { {
//            cname = $2;
//            extends = Parent($4);
//            cbody = $6;
//      }}
//
//cbody:
//      /* nothing */ { {
//          fields = [];
//      } }
    
/* Literals */
/* -------- */

literals:
      INT_LIT           { IntLit($1) }
    | FLOAT_LIT         { FloatLit($1) }
    | TRUE              { BoolLit(true) }
    | FALSE             { BoolLit(false) }
    | CHAR_LIT          { CharLit($1) }
    | STRING_LIT        { StringLit($1) }
    | ID                { Id($1) }

/* Statements */
/* ---------- */

stmts:
    | stmt_list             { List.rev $1 }
    
stmt_list:
      stmt                  { [$1] }
    | stmt_list stmt        { $2::$1 }

stmt:
      expr SEMI                     { Expr($1) }
    | RETURN SEMI                   { Return(Noexpr) }
    | RETURN expr SEMI              { Return($2) }
    | LBRACE stmt_list RBRACE       { Block($2) } 
    | IF LPAREN expr RPAREN stmt %prec NOELSE   
                                        { If($3, $5, Block([Expr(Noexpr)])) }
    | IF LPAREN expr RPAREN stmt ELSE stmt      
                                        { If($3, $5, $7) }
    | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt 
                                        { For($3, $5, $7, $9) }
    | WHILE LPAREN expr RPAREN stmt     
                                        { While($3, $5) }
    /* TODO: clarify declaration syntax */
    | datatype ID SEMI                  { Local($1, $2, Noexpr) }
    | datatype ID ASSIGN expr SEMI      { Local($1, $2, $4) }

/* Expressions */
/* ----------- */

expr_opt:
      /* nothing */         { Noexpr }
    | expr                  { $1 }

expr:
      literals                      { $1 }
    | expr PLUS     expr            { Binop($1, Add, $3) }
    | expr MINUS    expr            { Binop($1, Sub, $3) }
    | expr TIMES    expr            { Binop($1, Mult, $3) }
    | expr DIVIDE   expr            { Binop($1, Div, $3) }
    | expr EQ       expr            { Binop($1, Equal, $3) }
    | expr NEQ      expr            { Binop($1, Neq, $3) }
    | expr LT       expr            { Binop($1, Less, $3) }
    | expr LEQ      expr            { Binop($1, Leq, $3) }
    | expr GT       expr            { Binop($1, Greater, $3) }
    | expr GEQ      expr            { Binop($1, Geq, $3) }
    | expr AND      expr            { Binop($1, And, $3) }
    | expr OR       expr            { Binop($1, Or, $3) }
    | MINUS expr %prec NEG          { Unop(Neg, $2) } 
    | NOT expr                      { Unop(Not, $2) }
    | LPAREN expr RPAREN            { $2 }
    | ID LPAREN actuals_opt RPAREN  { Call($1, $3) }

%%
