(* Code generation: translate takes a semantically checked AST and
  produces LLVM IR 
       
LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/
     
*)    

open Ast
     
module L = Llvm
module A = Ast
      
module StringMap = Map.Make(String)

let translate ast = match ast with
    Program(includes, functions) -> 
    let context     = L.global_context () in 
    let the_module  = L.create_module context "Stop"
    and i32_t       = L.i32_type    context
    and i8_t        = L.i8_type     context
    and i1_t        = L.i1_type     context in

    the_module
