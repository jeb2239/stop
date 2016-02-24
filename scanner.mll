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
let ident = ['a'-'z' 'A'-'Z']
let ident_num = ['a'-'z' 'A'-'Z' '0'-'9']

rule token = parse
      [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
    | '#'       { comment lexbuf }          (* Comments *)
    | '\n'      { NEWLINE }
	| '('       { LPAREN }
	| ')'       { RPAREN }
    | '{'       { LBRACE }
    | '}'       { RBRACE }
    | ':'       { COLON }
    | ';'       { SEMI }
    | ','       { COMMA }
	| '+'       { PLUS }
	| '-'       { MINUS }
	| '*'       { TIMES }
	| '/'       { DIVIDE }
	| '='       { ASSIGN }
	| '^'       { CARET }
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
	|digit+
	|"." digit+
	| digit+ "." digit* as num { NUM (float_of_string num) }
	| ident ident_num* as word 
		{try
			let f = Hashtbl.find fun_table word in 
			FNCT f
			with Not_found -> VAR word
		}
	| eof { raise End_of_file }
	| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
'\n'      { token lexbuf }
| _         { comment lexbuf }
