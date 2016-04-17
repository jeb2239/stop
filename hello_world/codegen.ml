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
let named_values:(llvalue) String.Table.t = String.Table.create ()
let named_params:(llvalue) String.Table.t = String.Table.create ()
let struct_types:(lltype) String.Table.t = String.Table.create ()
let struct_field_indexes:(int) String.Table.t =  String.Table.create ()

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



let rec get_ptr_type datatype = match datatype with
		Arraytype(t, 0) -> get_type (Datatype(t))
	|	Arraytype(t, 1) -> pointer_type (get_type (Datatype(t)))
	|	Arraytype(t, i) -> pointer_type (get_ptr_type (Arraytype(t, (i-1))))
	| 	_ -> raise(Exceptions.InvalidStructType "Array Pointer Type")

and find_struct name = 
	try Hashtbl.find struct_types name
	with | Not_found -> raise(Exceptions.InvalidStructType name)

and get_type (datatype:Ast.datatype) = match datatype with 
		Datatype(Int_t) -> i32_t
	| 	Datatype(Float_t) -> f_t
	| 	Datatype(Bool_t) -> i1_t
	| 	Datatype(Char_t) -> i8_t
	| 	Datatype(Unit_t) -> void_t
	(*| 	Datatype(Null_t) -> i32_t*)
	| 	Datatype(Object_t(name)) -> pointer_type(find_struct name)
	(*|   Functiontype(dt_list,ret) -> *)
	| 	Arraytype(t, i) -> get_ptr_type (Arraytype(t, (i)))
	| 	d -> raise(Exceptions.InvalidStructType (Ast.string_of_datatype d)) 

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

let codegen_main main = 
	Hashtbl.clear named_values;
	Hashtbl.clear named_params;
	let fty = function_type i32_t [| i32_t; pointer_type str_t |] in
	let f = define_function "main" fty the_module in
	let llbuilder = builder_at_end context (entry_block f) in

	let argc = param f 0 in
	let argv = param f 1 in
	set_value_name "argc" argc; 
	set_value_name "argv" argv;
	let args = construct_args argc argv llbuilder in
	Hashtbl.add named_params "args" args;

	let _ = codegen_stmt llbuilder () in
	build_ret (const_int i32_t 0) llbuilder 


let codegen_sprogram sprogram = 
	let _ = codegen_library_functions () in
	debug "Stop";
	the_module