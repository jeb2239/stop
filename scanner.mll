{
	open Parser
	open Lexing

	let create_hashtable size init =
		let tbl = Hashtbl.create size in 
		List.iter (fun (key,data) -> Hashtbl.add tbl key data) init;
		tbl

	let fun_table =  create_hashtable 16 [
			("sin", sin);
			

	]
}

let digit =  ['0'-'9']
let ident = ['a'-'z' 'A'-'Z']
let ident_num = ['a'-'z' 'A'-'Z' '0'-'9']
rule token = parse
	| [' ' '\t'] {token lexbuf}
	| '\n' {NEWLINE}
	|digit+
	|"." digit+
	| digit+ "." digit* as num
	{ NUM (float_of_string num)}
	| '+' {PLUS}
	| '-' {MINUS}
	| '*' {MULT}
	| '/' {DIV}
	| '^' {CARET}
	| '(' {LPAREN}
	| ')' {RPAREN}
	| '=' {EQ}
	| ident ident_num* as word 
		{try
			let f = Hashtbl.find fun_table word in 
			FNCT f
			with Not_found -> VAR word
		}
	| _ {token lexbuf}
	|eof {raise End_of_file}
