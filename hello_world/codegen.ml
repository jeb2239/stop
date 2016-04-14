(* Code generation: translate takes a semantically checked AST and
  produces LLVM IR 
       
LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/
     
*)    

open Llvm
open Ast
open Core.Std

open Llvm.MemoryBuffer
open Llvm_bitreader



let context = global_context ()
let the_module = create_module context "Stop"
let builder = builder context 
let named_values:(string,llvalue) Hashtbl.t = Hashtbl.create 50
let named_params:(string,llvalue) Hashtbl.t = Hashtbl.create 50
let struct_types:(string,lltype) Hashtbl.t = Hashtbl.create 10
let struct_field_indexes:(string, int) Hashtbl.t = Hashtbl.create 50

