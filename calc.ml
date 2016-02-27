open Parser
open Lexing
open Scanner

let _ = 
    let lexbuf = Lexing.from_channel stdin in 
    Scanner.token lexbuf 
