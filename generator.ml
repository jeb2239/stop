open Parser
module E = Exceptions

type token_attr = {
    lineno : int;
    cnum : int;
}

(* Build an OCaml List of the tokens returned from the Scanner *)
let build_token_list filename lexbuf =
    Scanner.filename := filename;
    let rec helper lexbuf token_list =
        let token = Scanner.token lexbuf in
        match token with
            EOF as eof  -> eof::token_list
          | t           -> t::helper lexbuf token_list
    in
    helper lexbuf []

(* Build an AST by feeding the Scanner's tokens into the Parser *)
let build_ast filename token_list =
    let token_list = ref(token_list) in
    let tokenizer _ =
        match !token_list with
            head :: tail -> token_list := tail; head
          | [] -> raise E.MissingEOF
    in
    let program = Parser.program tokenizer (Lexing.from_string "") in
    program
