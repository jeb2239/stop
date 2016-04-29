(* Code Generation Phase *) 

(*
    Input: Semantically Checked AST (type sprogram)
    Output: LLVM Module 
       
    Produces an LLVM IR translation of the source program
    LLVM Tutorial:
        http://llvm.org/docs/tutorial/index.html
    LLVM Documentation:
        http://llvm.moe/
        http://llvm.moe/ocaml/
*)    

open Core.Std
open Sast 

module A = Ast
module E = Exceptions
module L = Llvm
module U = Utils
      
module StringMap = Map.Make(String)

let context     = L.global_context ()
let the_module  = L.create_module context "Stop"
let builder     = L.builder context
let i32_t       = L.i32_type context
let i8_t        = L.i8_type context
let i1_t        = L.i1_type context 
let str_t       = L.pointer_type (L.i8_type context)
let void_t      = L.void_type context 

let struct_types:(string, L.lltype) Hashtbl.t = Hashtbl.create ()
    ~hashable:String.hashable 
    ~size:10

let str_type = A.Arraytype(A.Char_t, 1)

let codegen_library_functions () = 
    (* C Std lib functions (Free with Llvm) *)
    let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    let _ = L.declare_function "printf" printf_t the_module in
    let malloc_t = L.function_type (str_t) [| i32_t |] in
    let _ = L.declare_function "malloc" malloc_t the_module in
    let open_t = L.function_type i32_t [| (L.pointer_type i8_t); i32_t |] in 
    let _ = L.declare_function "open" open_t the_module in
    let close_t = L.function_type i32_t [| i32_t |] in
    let _ = L.declare_function "close" close_t the_module in
    let read_t = L.function_type i32_t [| i32_t; L.pointer_type i8_t; i32_t |] in
    let _ = L.declare_function "read" read_t the_module in
    let write_t = L.function_type i32_t [| i32_t; L.pointer_type i8_t; i32_t |] in
    let _ = L.declare_function "write" write_t the_module in 
    let lseek_t = L.function_type i32_t [| i32_t; i32_t; i32_t |] in
    let _ = L.declare_function "lseek" lseek_t the_module in
    let exit_t = L.function_type void_t [| i32_t |] in
    let _ = L.declare_function "exit" exit_t the_module in
    let realloc_t = L.function_type str_t [| str_t; i32_t |] in
    let _ = L.declare_function "realloc" realloc_t the_module in
    let getchar_t = L.function_type (i32_t) [| |] in
    let _ = L.declare_function "getchar" getchar_t the_module in
    ()

let codegen_struct_stub s =
    let struct_t = L.named_struct_type context s.scname
    in
    Hashtbl.add struct_types
        ~key:s.scname
        ~data:struct_t

let codegen_struct s =
    let struct_t = Hashtbl.find_exn struct_types s.scname in
    ()

let codegen_sast sast =
    (* Declare the various LLVM Reserved Functions *)
    let _ = codegen_library_functions ()
    in
    (* Generate a map of class names to their respective LLVM Struct Types *)
    let _ = List.map sast.classes 
        ~f:(fun s -> codegen_struct_stub s)
    in
    (* Generate LLVM IR for classes *)
    let _ = List.map sast.classes
        ~f:(fun s -> codegen_struct s)
    in
    the_module
