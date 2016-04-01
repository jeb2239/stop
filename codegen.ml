(* Code generation: translate takes a semantically checked AST and
  produces LLVM IR 
       
LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/
     
*)    

module A = Ast
module L = Llvm
module U = Utils

module E = Exceptions
      
module StringMap = Map.Make(String)

let translate ast = match ast with
    A.Program(includes, functions) -> 
    let context     = L.global_context () in 
    let the_module  = L.create_module context "Stop"
    and i64_t       = L.i64_type    context
    and i32_t       = L.i32_type    context
    and i8_t        = L.i8_type     context
    and i1_t        = L.i1_type     context 
    and str_t       = L.pointer_type (L.i8_type context)
    and void_t      = L.void_type   context in

    let str_type = A.Arraytype(A.Char_t, 1) in 
    
    let ltype_of_prim = function
        A.Int_t ->          i32_t
      | A.Float_t ->        i32_t
      | A.Bool_t ->         i1_t
      | A.Char_t ->         i8_t
      | A.Unit_t ->         void_t
      (* TODO: Implement find_struct function for Object_t *)
      | A.Object_t(s) ->    L.pointer_type i8_t
    in

    let rec ltype_of_arraytype arraytype = match arraytype with
        A.Arraytype(p, 1) -> L.pointer_type (ltype_of_prim p)
      | A.Arraytype(p, i) -> 
            L.pointer_type (ltype_of_arraytype (A.Arraytype(p, i-1)))
      | _ -> raise(E.InvalidStructType "Array Pointer Type")
    in

    let ltype_of_datatype = function
        A.Datatype(p) -> ltype_of_prim p
      | A.Arraytype(p, i) -> ltype_of_arraytype (A.Arraytype(p,i)) in

    let ltype_of_formal = function
        A.Formal(data_t, s) -> ltype_of_datatype data_t in
    
    let atype_of_datatype = function
        A.Datatype(p) -> p 
      | A.Arraytype(p, i) -> p in

    (* Declare printf(), which the print built-in function will call *)
    (* printf() is already implemented in LLVM *)
    let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    let printf_func = L.declare_function "printf" printf_t the_module in

    (* Define each function (arguments and return type) so we can call it *)
    let function_decls =
        let function_decl m fdecl =
            let name = U.string_of_fname fdecl.A.fname
            and formal_types =
        Array.of_list (List.map (fun formal -> ltype_of_formal formal) fdecl.A.formals)
            in let ftype = L.function_type (ltype_of_datatype fdecl.A.return_t) formal_types in
            StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

    (* Fill in the body of the given function *)
    let build_function_body fdecl =
        let (the_function, _) = StringMap.find (U.string_of_fname fdecl.A.fname) function_decls in
        let builder = L.builder_at_end context (L.entry_block the_function) in

        let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in

        (* Construct code for an expression; return its value *)
        let rec expr builder = function
            A.IntLit i -> L.const_int i32_t i
          | A.Call ("printf", [e]) ->
                  L.build_call printf_func [| int_format_str ; (expr builder e) |] "printf" builder 
        in

        (* Invoke "f builder" if the current block doesn't already
           have a terminal (e.g., a branch). *)
        let add_terminal builder f =
            match L.block_terminator (L.insertion_block builder) with
                Some _ -> ()
              | None -> ignore (f builder) in

        (* Build the code for the given statement; return the builder for
           the statement's successor *)
        let rec stmt builder = function
            A.Block sl -> List.fold_left stmt builder sl
          | A.Expr e -> ignore (expr builder e); builder 
          | A.Return e -> 
                ignore (match (ltype_of_datatype fdecl.A.return_t) with
                    void_t -> L.build_ret_void builder
                  | _ -> L.build_ret (expr builder e) builder); builder
        in

        (* Build the code for each statement in the function *)
        let builder = stmt builder (A.Block fdecl.A.body) in

        (* Add a return if the last block falls off the end *)
        add_terminal builder (match (ltype_of_datatype fdecl.A.return_t) with
            void_t -> L.build_ret_void
          | ltype -> L.build_ret (L.const_int ltype 0))
    in

    List.iter build_function_body functions;
    the_module
