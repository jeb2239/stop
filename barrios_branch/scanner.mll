(* Ocamllex scanner for Stop Language *)

{ open Parser }	

let digit = ['0'-'9']
let exp = (('e'|'E')('-'|'+')?digit+)

let alpha = ['a'-'z' 'A'-'Z']
let upper_alpha = ['A'-'Z']
let lower_alpha = ['a'-'z']

(* Regex for C-Style Float *)
let flot = (digit+'.'digit*exp?)|(digit+'.'?digit*exp)|(digit*'.'digit+exp?)|(digit*'.'?digit+exp)

rule token = parse
      [' ' '\t' '\r' '\n'] { token lexbuf }         (* Whitespace *)
    | "//"      { single_comment lexbuf }           (* Comments *)
    | "/*"      { multi_comment lexbuf }
    | '\n'      { NEWLINE }
	| '('       { LPAREN }
	| ')'       { RPAREN }
    | '{'       { LBRACE }
    | '}'       { RBRACE }
    | '['		{ LBRACKET }
    | ']'		{ RBRACKET }
    | ':'       { COLON }
    | ';'       { SEMI }
    | ','       { COMMA }
    |  '.'      {DOT}
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
    | "var"     { VAR }
    (* Self referential kw*) 
    | "type"    { TYPE }
    | "this"    { THIS }

    (* Conditionals *)
    | "if"      { IF }
    | "elseif"  { ELSEIF }
    | "else"    { ELSE }
    | "for"     { FOR }
    | "while"   { WHILE }
    | "return"  { RETURN }
    | "->"      { ARROW }
    (* Reserved *)
    | "method"      {METHOD}
    | "spec"        { SPEC }
    | "def"		    { DEF }
    | "class"	    { CLASS }
    | "#include"    { INCLUDE }
    (*access mods*)
    | "pub" { PUB }
    | "priv" { PRIV }
    | '@'   {ANON}
    | "pattern" {PATTERN}
    | "function"     {FUNCTION}
    | "Fun"         {FUN}

    (* PRIMITIVES *)
    | "Int"     { INT }
    | "Float"   { FLOAT }
    | "Bool"    { BOOL }
    | "Char"    { CHAR }

    (* PRIMITIVE LITERALS *)
    | "Unit"    { UNIT }
    | "true"    { TRUE }
    | "false"   { FALSE }
    | digit+                    as lit { INT_LIT(int_of_string lit) }
    | flot                      as lit { FLOAT_LIT(float_of_string lit) }
    | '"'(alpha* as s)'"'       { STRING_LIT(s) }
    | (lower_alpha)(alpha)+     as lit { ID(lit) }
    | (upper_alpha)(alpha)+     as lit { TYPE_ID(lit) }
	| eof { EOF }
	| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and single_comment = parse
      '\n'      { token lexbuf }
    | _         { single_comment lexbuf }

and multi_comment = parse
      "*/"    { token lexbuf }
    | _     { multi_comment lexbuf }
