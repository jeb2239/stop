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
open Ast

module A = Analysis
module E = Exceptions
module L = Llvm
module U = Utils
      
let context     = L.global_context ()
let the_module  = L.create_module context "Stop"
let builder     = L.builder context
let i32_t       = L.i32_type context
let i8_t        = L.i8_type context
let i1_t        = L.i1_type context 
let float_t     = L.float_type context
let double_t    = L.double_type context
let void_t      = L.void_type context 

let str_t       = L.pointer_type (L.i8_type context)

let struct_types:(string, L.lltype) Hashtbl.t = Hashtbl.create ()
    ~hashable:String.hashable 
    ~size:10

let struct_field_indexes:(string, int) Hashtbl.t = Hashtbl.create ()
    ~hashable:String.hashable 
    ~size:50

(* Named Values inside current scope *)
let named_values:(string, L.llvalue) Hashtbl.t = Hashtbl.create ()
    ~hashable:String.hashable 
    ~size:50

(* Named parameters inside current scope *)
let named_parameters:(string, L.llvalue) Hashtbl.t = Hashtbl.create ()
    ~hashable:String.hashable 
    ~size:50

let str_type = Arraytype(Char_t, 1)

let rec get_array_type array_t = match array_t with
    Arraytype(prim, 1) -> L.pointer_type(get_lltype_exn (Datatype(prim)))
  | Arraytype(prim, i) -> L.pointer_type(get_array_type (Arraytype(prim, i-1)))
  | _ -> raise(E.InvalidDatatype "Array Type")

and find_struct_exn name =
    try
        Hashtbl.find_exn struct_types name
    with
        Not_found -> raise (E.InvalidStructType(name))

and get_function_type data_t_list return_t =
    let llargs = List.fold_left data_t_list
        ~f:(fun l data_t -> get_lltype_exn data_t :: l)
        ~init:[]
    in
    L.function_type (get_lltype_exn return_t) (Array.of_list llargs)

and get_lltype_exn (data_t:datatype) = match data_t with
    Datatype(Int_t) -> i32_t
  | Datatype(Float_t) -> double_t (* TODO: Decide what to do a/b doubles & floats *)
  | Datatype(Bool_t) -> i1_t
  | Datatype(Char_t) -> i8_t
  | Datatype(Unit_t) -> void_t
  | Datatype(Object_t(name)) -> L.pointer_type(find_struct_exn name)
  | Arraytype(t, i) -> get_array_type (Arraytype(t, i))
  | Functiontype(dt_l, dt) -> get_function_type dt_l dt
  | data_t -> raise (E.InvalidDatatype(U.string_of_datatype data_t))

let lookup_llfunction_exn fname = match (L.lookup_function fname the_module) with
    None -> raise (E.LLVMFunctionNotFound(fname))
  | Some f -> f

(* Generate Code for Binop *)
let rec handle_binop e1 op e2 data_t llbuilder = 
    (* Get the types of e1 and e2 *)
    let type1 = A.sexpr_to_type e1 in
    let type2 = A.sexpr_to_type e2 in

    (* Generate llvalues from e1 and e2 *)
    let e1 = codegen_sexpr e1 ~builder:llbuilder in
    let e2 = codegen_sexpr e2 ~builder:llbuilder in

    (* Integer Llvm functions *)
    let int_ops e1 op e2 =
        match op with
            Add     -> L.build_add e1 e2 "addtmp" llbuilder
          | Sub     -> L.build_sub e1 e2 "subtmp" llbuilder
          | Mult    -> L.build_mul e1 e2 "multmp" llbuilder
          | Div     -> L.build_sdiv e1 e2 "divtmp" llbuilder
          | Modulo  -> L.build_srem e1 e2 "sremtmp" llbuilder
          | Equal   -> L.build_icmp L.Icmp.Eq e1 e2 "eqtmp" llbuilder
          | Neq     -> L.build_icmp L.Icmp.Ne e1 e2 "neqtmp" llbuilder
          | Less    -> L.build_icmp L.Icmp.Slt e1 e2 "lesstmp" llbuilder
          | Leq     -> L.build_icmp L.Icmp.Sle e1 e2 "leqtmp" llbuilder
          | Greater -> L.build_icmp L.Icmp.Sgt e1 e2 "sgttmp" llbuilder
          | Geq     -> L.build_icmp L.Icmp.Sge e1 e2 "sgetmp" llbuilder
          | And     -> L.build_and e1 e2 "andtmp" llbuilder
          | Or      -> L.build_or  e1 e2 "ortmp" llbuilder
          | _       -> raise Exceptions.IntOpNotSupported
    in

    (* Floating Point Llvm functions *)
    let float_ops e1 op e2 =
        match op with
            Add     -> L.build_fadd e1 e2 "flt_addtmp" llbuilder
          | Sub     -> L.build_fsub e1 e2 "flt_subtmp" llbuilder
          | Mult    -> L.build_fmul e1 e2 "flt_multmp" llbuilder
          | Div     -> L.build_fdiv e1 e2 "flt_divtmp" llbuilder
          | Modulo  -> L.build_frem e1 e2 "flt_sremtmp" llbuilder
          | Equal   -> L.build_fcmp L.Fcmp.Oeq e1 e2 "flt_eqtmp" llbuilder
          | Neq     -> L.build_fcmp L.Fcmp.One e1 e2 "flt_neqtmp" llbuilder
          | Less    -> L.build_fcmp L.Fcmp.Ult e1 e2 "flt_lesstmp" llbuilder
          | Leq     -> L.build_fcmp L.Fcmp.Ole e1 e2 "flt_leqtmp" llbuilder
          | Greater -> L.build_fcmp L.Fcmp.Ogt e1 e2 "flt_sgttmp" llbuilder
          | Geq     -> L.build_fcmp L.Fcmp.Oge e1 e2 "flt_sgetmp" llbuilder
          | _       -> raise Exceptions.FloatOpNotSupported
    in

    (* TODO: Handle Casting *)

    (* Use Integer Arithmetic for Ints, Chars, and Bools *)
    (* Use Floating-Point Arithmetic for Floats *)
    let type_handler data_t = match data_t with
        Datatype(Int_t) 
      | Datatype(Char_t)
      | Datatype(Bool_t) -> int_ops e1 op e2
      | Datatype(Float_t) -> float_ops e1 op e2
      | _ -> raise E.InvalidBinopEvaluationType
    in
    type_handler data_t

and codegen_call fname sexpr_l data_t llbuilder = match fname with
    "printf" -> codegen_printf sexpr_l llbuilder
  | _ as fname -> codegen_function_call fname sexpr_l data_t llbuilder

and codegen_function_call fname sexpr_l data_t llbuilder =
    let f = lookup_llfunction_exn fname in
    let params = List.map ~f:(codegen_sexpr ~builder:llbuilder) sexpr_l in
    match data_t with
        Datatype(Unit_t) -> L.build_call f (Array.of_list params) "" llbuilder
    |   _ -> L.build_call f (Array.of_list params) "tmp" llbuilder

and codegen_printf sexpr_l llbuilder =
    (* Convert printf format string to llvalue *)
    let format_str = List.hd_exn sexpr_l in
    let format_llstr = match format_str with
        SStringLit(s) -> L.build_global_stringptr s "fmt" llbuilder
      | _ -> raise E.PrintfFirstArgNotString
    in
    (* Convert printf args to llvalue *)
    let args = List.tl_exn sexpr_l in
    let format_llargs = List.map args ~f:(codegen_sexpr ~builder:llbuilder) in
    (* Build printf call *)
    let fun_llvalue = lookup_llfunction_exn "printf" in
    let llargs = Array.of_list (format_llstr :: format_llargs) in
    L.build_call fun_llvalue llargs "printf" llbuilder

and codegen_id id llbuilder = 
    try Hashtbl.find_exn named_parameters id
    with | Not_found ->
        try let var = Hashtbl.find_exn named_values id in
            L.build_load var id llbuilder 
        with | Not_found -> raise (E.UndefinedId id)

and codegen_assign e1 e2 llbuilder =
    (* Get lhs llvalue; don't emit as expression *)
    let lhs = match e1 with
        SId(id, _) -> 
            try Hashtbl.find_exn named_parameters id
            with | Not_found ->
                try Hashtbl.find_exn named_values id
                with | Not_found -> raise (E.UndefinedId id)
      | _ -> raise E.AssignmentLhsMustBeAssignable
    in
    (* Get rhs llvalue *)
    let rhs = match e2 with 
      | _ -> codegen_sexpr e2 llbuilder 
    in
    (* Codegen Assignment Stmt *)
    ignore(L.build_store rhs lhs llbuilder);
    rhs

and codegen_array_access isAssign e e_l llbuilder =
    let indices = List.map e_l ~f:(codegen_sexpr ~builder:llbuilder) in
    let indices = Array.of_list indices in
    let arr = codegen_sexpr e llbuilder in
    let llvalue = L.build_gep arr indices "tmp" llbuilder in
    if isAssign
        then llvalue
        else L.build_load llvalue "tmp" llbuilder

and codegen_sexpr sexpr ~builder:llbuilder = match sexpr with 
    SIntLit(i)                  -> L.const_int i32_t i
  | SFloatLit(f)                -> L.const_float float_t f
  | SBoolLit(b)                 -> if b then L.const_int i1_t 1 else L.const_int i1_t 0
  | SCharLit(c)                 -> L.const_int i8_t (Char.to_int c)
  | SStringLit(s)               -> L.build_global_stringptr s "tmp" llbuilder
  | SId(id, _)                      -> codegen_id id llbuilder
  | SBinop(e1, op, e2, data_t)      -> handle_binop e1 op e2 data_t llbuilder
  | SCall(fname, se_l, data_t, _)   -> codegen_call fname se_l data_t llbuilder
  | SAssign(e1, e2, _)         -> codegen_assign e1 e2 llbuilder
  | SArrayAccess(e, e_l, _)    -> codegen_array_access false e e_l llbuilder
(*
  | SNoexpr                     -> build_add (const_int i32_t 0) (const_int i32_t 0) "nop" llbuilder
  | SArrayCreate(t, el, d)      -> codegen_array_create llbuilder t d el
  | SObjAccess(e1, e2, d)       -> codegen_obj_access true e1 e2 d llbuilder
  | SObjectCreate(id, el, d)    -> codegen_obj_create id el d llbuilder
  | SArrayPrimitive(el, d)      -> codegen_array_prim d el llbuilder
  | SUnop(op, e, d)             -> handle_unop op e d llbuilder
  | SNull                       -> const_null i32_t
  | SDelete e                   -> codegen_delete e llbuilder
*)

and codegen_return sexpr llbuilder = match sexpr with
    SNoexpr -> L.build_ret_void llbuilder
  | _ -> L.build_ret (codegen_sexpr sexpr llbuilder) llbuilder

(* TODO: Resolve this mess *)
(* Use L.build_load and L.build_store *)
and codegen_local var_name data_t sexpr llbuilder = 
    let lltype = match data_t with
        Datatype(Object_t(name)) -> find_struct_exn name
      | _ -> get_lltype_exn data_t
    in
    let malloc = L.build_malloc lltype var_name llbuilder in
    Hashtbl.add_exn named_values ~key:var_name ~data:malloc;
    let lhs = SId(var_name, data_t) in
    match sexpr with
        SNoexpr -> malloc
      | _ -> codegen_assign lhs sexpr llbuilder

and codegen_stmt llbuilder = function
    SBlock sl               -> List.hd_exn (List.map sl ~f:(codegen_stmt llbuilder))
  | SExpr(se, _)            -> codegen_sexpr se llbuilder
  | SReturn(se, _)     -> codegen_return se llbuilder
  | SLocal(s, data_t, se)   -> codegen_local s data_t se llbuilder
(*
  | SIf(e, s1, s2)          -> codegen_if_stmt e s1 s2 llbuilder
  | SFor(e1, e2, e3, s)     -> codegen_for e1 e2 e3 s llbuilder
  | SWhile(e, s)            -> codegen_while e s llbuilder
  | SBreak                  -> codegen_break llbuilder
  | SContinue               -> codegen_continue llbuilder
  | SLocal(d, s, e)         -> codegen_alloca d s e llbuilder
*)

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
    let struct_t = Hashtbl.find_exn struct_types s.scname
    in
    let type_list = List.map s.sfields 
        ~f:(function Field(_, _, data_t) -> get_lltype_exn data_t)
    in
    let name_list = List.map s.sfields
        ~f:(function Field(_, s, _) -> s)  
    in

    (* Add key field to all structs *)
    let type_list = i32_t :: type_list
    in
    let name_list = ".key" :: name_list 
    in

    let type_array = (Array.of_list type_list) 
    in
    List.iteri name_list
        ~f:(fun i f -> 
            let n = s.scname ^ "." ^ f in
            Hashtbl.add_exn struct_field_indexes ~key:n ~data:i);
    (* Add the struct to the module *)
    L.struct_set_body struct_t type_array true

let codegen_function_stub sfdecl =
    let fname = sfdecl.sfname
    in
    let is_var_arg = ref false 
    in
    let params = List.rev 
        (List.fold_left sfdecl.sformals
            ~f:(fun l -> (function 
                Formal(_, data_t) -> get_lltype_exn data_t :: l
              | _ -> is_var_arg := true; l))
            ~init: [])
    in
    let ftype = 
        if !is_var_arg
        then L.var_arg_function_type (get_lltype_exn sfdecl.sreturn_t) (Array.of_list params)
        else L.function_type (get_lltype_exn sfdecl.sreturn_t) (Array.of_list params)
    in
    L.define_function fname ftype the_module

let init_params f formals =
    let formals = Array.of_list formals
    in
    Array.iteri (L.params f)
        ~f:(fun i element ->
            let n = formals.(i)
            in
            let n = U.string_of_formal_name n 
            in
            L.set_value_name n element;
            Hashtbl.add_exn named_parameters
                ~key:n
                ~data:element;
            )

let codegen_function sfdecl =
    Hashtbl.clear named_values;
    Hashtbl.clear named_parameters;
    let fname = sfdecl.sfname
    in
    let f = lookup_llfunction_exn fname 
    in
    let llbuilder = L.builder_at_end context (L.entry_block f)
    in
    let _ = init_params f sfdecl.sformals
    in
    (* TODO: Handle overriding functions *)
    let _ = codegen_stmt llbuilder (SBlock(sfdecl.sbody)) 
    in
    if sfdecl.sreturn_t = Datatype(Unit_t)
    then ignore(L.build_ret_void llbuilder);
    ()

(* TODO: Figure out how to do argc/argv properly *)
(*
let construct_main_args argc argv llbuilder =
    let str_pt = L.pointer_type str_t 
    in
    let size_real = L.build_add argc (L.const_int i32_t 1) "arr_size" llbuilder
    in

    let arr = L.build_array_malloc str_pt size_real "args" llbuilder 
    in
    let arr = L.build_pointercast arr str_pt "args" llbuilder
    in
*)

let codegen_main main =
    Hashtbl.clear named_values;
    Hashtbl.clear named_parameters;
    let ftype = L.function_type i32_t [| i32_t; L.pointer_type str_t |] 
    in
    let f = L.define_function "main" ftype the_module
    in
    let llbuilder = L.builder_at_end context (L.entry_block f)
    in
    let argc = L.param f 0 
    in
    let argv = L.param f 1 
    in
    L.set_value_name "argc" argc;
    L.set_value_name "argv" argv;
    Hashtbl.add_exn named_parameters ~key:"argc" ~data:argc;
    Hashtbl.add_exn named_parameters ~key:"argv" ~data:argv;
    (*
    let args = construct_main_args argc argv llbuilder 
    in
    *)
    (* Generate LLVM IR for statements in function body *)
    let _ = codegen_stmt llbuilder (SBlock(main.sbody))
    in

    (* Check to make sure we return; add a return statement if not *)
    let last_bb = match (L.block_end (lookup_llfunction_exn "main")) with
        L.After(block) -> block
      | L.At_start(_) -> raise (E.FunctionWithoutBasicBlock("main"))
    in
    match (L.instr_end last_bb) with
        L.After(instr) ->
            let op = L.instr_opcode instr in
            if op = L.Opcode.Ret 
            then ()
            else ignore(L.build_ret (L.const_int i32_t 0) llbuilder); ()
      | L.At_start(_) -> ignore(L.build_ret (L.const_int i32_t 0) llbuilder); ()

let codegen_sast sast =
    (* Declare the various LLVM Reserved Functions *)
    let _ = codegen_library_functions ()
    in
    (* Generate a map of class names to their respective LLVM Struct Types *)
    let _ = List.map sast.classes ~f:(fun s -> codegen_struct_stub s)
    in
    (* Generate LLVM IR for classes *)
    let _ = List.map sast.classes ~f:(fun s -> codegen_struct s)
    in
    (* Define the program functions *)
    let _ = List.map sast.functions ~f:(fun f -> codegen_function_stub f)
    in
    (* Generate LLVM IR for functions *)
    let _ = List.map sast.functions ~f:(fun f -> codegen_function f)
    in
    (* Generate LLVM IR for main function *)
    let _ = codegen_main sast.main
    in
    the_module
