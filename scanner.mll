(* Ocamllex scanner for Stop Language *)

{
	open Parser
	open Lexing

	let create_hashtable size init =
		let tbl = Hashtbl.create size in 
		List.iter (fun (key,data) -> Hashtbl.add tbl key data) init;
		tbl

	let fun_table =  create_hashtable 16 [ ("sin", sin); ]
}

let digit =  ['0'-'9']
let exp = (('e'|'E')('-'|'+')?digit+)

(* Regex for C-Style Float *)
let flot = (digit+'.'digit*exp?)|(digit+'.'?digit*exp)|(digit*'.'digit+exp?)|(digit*'.'?digit+exp)

rule token = parse
      [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
    | "//"      { comment lexbuf }          (* Comments *)
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
    (* Conditionals *)
    | "if"      { IF }
    | "else if" { ELSEIF }
    | "else"    { ELSE }
    | "for"     { FOR }
    | "while"   { WHILE }
    | "return"  { RETURN }
    | "def"		{ DEF }
    | "class"	{ CLASS }
    | "Unit"	{ UNIT }
    (* PRIMITIVES *)
    | "int"     { INT }
    | "float"   { FLOAT }
    | "bool"    { BOOL }
    (* PRIMITIVE LITERALS *)
    | "true"|"false"        as lit { BOOL_LIT(bool_of_string lit) }
    | digit+                as lit { INT_LIT(int_of_string lit) }
    | flot                  as lit { FLOAT_LIT(float_of_string lit) }
	| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }
	| eof { raise End_of_file }

and comment = parse
'\n'      { token lexbuf }
| _         { comment lexbuf }
