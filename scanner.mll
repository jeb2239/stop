(* Ocamllex scanner for Stop Language *)

{ open Parser }	

let digit = ['0'-'9']
let alpha = ['a'-'z']
let upper_alpha = ['A'-'Z']

let exp = (('e'|'E')('-'|'+')?digit+)

(* Regex for C-Style Float *)
let flot = (digit+'.'digit*exp?)|(digit+'.'?digit*exp)|(digit*'.'digit+exp?)|(digit*'.'?digit+exp)

rule token = parse
      [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
    | "//"      { single_comment lexbuf }          (* Comments *)
    | "/*"      { multi_comment lexbuf }
    | '\n'      { NEWLINE }
	| '('       { LPAREN }
	| ')'       { RPAREN }
    | '{'       { LBRACE }
    | '}'       { RBRACE }
    | '['		{ LSQUARE }
    | ']'		{ RSQUARE }
    | ':'       { COLON }
    | ';'       { SEMI }
    | ','       { COMMA }
    (* Operators *)
	| '+'       { PLUS }
	| '-'       { MINUS }
	| '*'       { TIMES }
	| '/'       { DIVIDE }
	| '='       { ASSIGN }
	| '^'       { CARET }
    | '%'		{ MODULO }
    | "=="      { EQ }
    | "!="      { NEQ }
    | '<'       { LT }
    | "<="      { LEQ }
    | ">"       { GT }
    | ">="      { GEQ }
    | "&&"      { AND }
    | "||"      { OR }
    | "!"       { NOT }
    (* Modifiers *)
    | "final"   { FINAL } 
    | "type"    { TYPE }
    (* Conditionals *)
    | "if"      { IF }
    | "elseif"  { ELSEIF }
    | "else"    { ELSE }
    | "for"     { FOR }
    | "while"   { WHILE }
    | "return"  { RETURN }
    (* *)
    | "def"		{ DEF }
    | "class"	{ CLASS }
    | "Unit"	{ UNIT }
    (* PRIMITIVES *)
    | "int"     { INT }
    | "float"   { FLOAT }
    | "bool"    { BOOL }
    (* PRIMITIVE LITERALS *)
    | "true"    { TRUE }
    | "false"   { FALSE }
    | digit+                as lit { INT_LIT(int_of_string lit) }
    | flot                  as lit { FLOAT_LIT(float_of_string lit) }
    | upper_alpha+alpha+    as lit { TYPE_ID(lit) }
	| eof { EOF  }
	| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and single_comment = parse
      '\n'      { token lexbuf }
    | _         { single_comment lexbuf }

and multi_comment = parse
      "*/"    { token lexbuf }
    | _     { multi_comment lexbuf }
