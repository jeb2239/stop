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
    | "if"      { IF }
    | "else"    { ELSE }
    | "for"     { FOR }
    | "while"   { WHILE }
    | "return"  { RETURN }
    | "def"		{ DEF }
    | "class"	{ CLASS }
    | "Unit"	{ UNIT }
    | ""
	|digit+
	|"." digit+
	| digit+ "." digit* as num { FLOAT(float_of_string num) }
	| eof { raise End_of_file }
	| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
'\n'      { token lexbuf }
| _         { comment lexbuf }
