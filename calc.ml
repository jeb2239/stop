open Parser
open Lexing
open Scanner

let _ = 
    let lexbuf = Lexing.form_channel stdin in 
    Printexc.print main ()
