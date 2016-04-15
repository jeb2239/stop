(* Code generation: translate takes a semantically checked AST and
  produces LLVM IR 
       
LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/
     
*)    

open Llvm
(*open Ast*)
open Core.Std

open Llvm.MemoryBuffer
open Llvm_bitreader



let context = global_context ()
let the_module = create_module context "Stop"
let builder = builder context 
let named_values = String.Table.create ()
let named_params = String.Table.create ()
let struct_types = String.Table.create ()
let struct_field_indexes =  String.Table.create ()

let i32_t = i32_type context;;
let i8_t = i8_type context;;
let f_t = double_type context;;
let i1_t = i1_type context;;
let str_t = pointer_type i8_t;;
let i64_t = i64_type context;;
let void_t = void_type context;;

let (br_block) = ref (block_of_value (const_int i32_t 0))
let (cont_block) = ref (block_of_value (const_int i32_t 0))
let is_loop = ref false

let debug = fun s ->  
	print_endline ("`````````````````````````````````````"^s);
	dump_module the_module;
	print_endline ("`````````````````````````````````````"^s);
	()

let codegen_library_functions () = 
	(* C Std lib functions *)
	let printf_ty = var_arg_function_type i32_t [| pointer_type i8_t |] in
	let _ = declare_function "printf" printf_ty the_module in
	let malloc_ty = function_type (str_t) [| i32_t |] in
	let _ = declare_function "malloc" malloc_ty the_module in
    let open_ty = function_type i32_t [| (pointer_type i8_t); i32_t |] in 
    let _ = declare_function "open" open_ty the_module in
    let close_ty = function_type i32_t [| i32_t |] in
    let _ = declare_function "close" close_ty the_module in
    let read_ty = function_type i32_t [| i32_t; pointer_type i8_t; i32_t |] in
    let _ = declare_function "read" read_ty the_module in
    let write_ty = function_type i32_t [| i32_t; pointer_type i8_t; i32_t |] in
    let _ = declare_function "write" write_ty the_module in 
    let lseek_ty = function_type i32_t [| i32_t; i32_t; i32_t |] in
    let _ = declare_function "lseek" lseek_ty the_module in
    let exit_ty = function_type void_t [| i32_t |] in
    let _ = declare_function "exit" exit_ty the_module in
	let realloc_ty = function_type str_t [| str_t; i32_t |] in
	let _ = declare_function "realloc" realloc_ty the_module in
    let getchar_ty = function_type (i32_t) [| |] in
    let _ = declare_function "getchar" getchar_ty the_module in

	(* Dice defined functions *)
	let fty = function_type (pointer_type i64_t) [| i32_t; i32_t |] in
	let _ = define_function "lookup" fty the_module in
    let rec_init_ty = function_type void_t [| (pointer_type i64_t); i32_t; (pointer_type i32_t); (pointer_type i32_t); (pointer_type i32_t); i32_t; i32_t |] in
    let _ = declare_function "rec_init" rec_init_ty the_module in
    let init_arr_ty = function_type (pointer_type i64_t) [| (pointer_type i32_t); i32_t |] in
    let _ = declare_function "init_arr" init_arr_ty the_module in
    let input_ty = function_type str_t [||] in
    let _ = declare_function "input" input_ty the_module in
    ()


let codegen_sprogram sprogram = 
	let _ = codegen_library_functions () in
	debug "Stop";
	the_module