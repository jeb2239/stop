(* Semantic Analyer for Stop Language *)

open Ast

let analyze filename ast = match ast with
    Program(includes, specs, cdecls, fdecls) ->
        let sast = "" in
        sast
