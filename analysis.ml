(* Semantic Analyzer for Stop Language *)

open Ast
open Sast

module G = Generator
module U = Utils
module E = Exceptions

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

(* HashMap which contains information re: Classes *)
type class_map = {
    field_map       : Ast.field StringMap.t;
    method_map      : Ast.fdecl  StringMap.t;
    (* constructor_map : Ast.fdecl StringMap.t; *)
    reserved_map    : Sast.sfdecl StringMap.t;
    cdecl           : Ast.cdecl;
}

(* Analysis Environment for functions *)
type env = {
    env_class_maps  : class_map StringMap.t;
    env_name        : string;
    env_cmap        : class_map;
    env_locals      : datatype StringMap.t;
    env_parameters  : Ast.formal StringMap.t;
    env_return_t    : datatype;
    env_in_for      : bool;
    env_in_while    : bool;
    env_reserved    : sfdecl list;
}

let update_env_name env env_name =
{
    env_class_maps  = env.env_class_maps;
    env_name        = env_name;
    env_cmap        = env.env_cmap;
    env_locals      = env.env_locals;
    env_parameters  = env.env_parameters;
    env_return_t    = env.env_return_t;
    env_in_for      = env.env_in_for;
    env_in_while    = env.env_in_while;
    env_reserved    = env.env_reserved;
}

let update_call_stack env in_for in_while =
{
    env_class_maps  = env.env_class_maps;
    env_name        = env.env_name;
    env_cmap        = env.env_cmap;
    env_locals      = env.env_locals;
    env_parameters  = env.env_parameters;
    env_return_t    = env.env_return_t;
    env_in_for      = in_for;
    env_in_while    = in_while;
    env_reserved    = env.env_reserved;
}

(* Name all methods <cname>.<fname> *)
let get_method_name cname fdecl =
    let name = fdecl.fname in
    cname ^ "." ^ name 

let add_reserved_functions =
    let reserved_stub fname return_t formals =
        {
            sfname      = fname;
            sreturn_t   = return_t;
            sformals    = formals;
            sbody       = [];
            fgroup      = Sast.Reserved;
            overrides   = false;
            source      = "NA";
        }
    in
    let i32_t = Datatype(Int_t) in
    let void_t = Datatype(Unit_t) in
    let str_t = Arraytype(Char_t, 1) in
    let formal_of s data_t = Formal(s, data_t) in
    let reserved = [
        reserved_stub "printf"  (void_t) ([Many(Any)]);
        reserved_stub "malloc"  (str_t)  ([formal_of "size" i32_t ]);
        reserved_stub "cast"    (Any)    ([formal_of "in" Any]);
        reserved_stub "sizeof"  (i32_t)  ([formal_of "in" Any]);
        reserved_stub "open"    (i32_t)  ([formal_of "path" str_t; formal_of "flags" i32_t]);
        reserved_stub "close"   (i32_t)  ([formal_of "fd" i32_t]);
        reserved_stub "read"    (i32_t)  ([formal_of "fd" i32_t; 
                                            formal_of "buf" str_t; formal_of "nbyte" i32_t]);
        reserved_stub "write"   (i32_t)  ([formal_of "fd" i32_t; 
                                            formal_of "buf" str_t; formal_of "nbyte" i32_t]);
        reserved_stub "lseek"   (i32_t)  ([formal_of "fd" i32_t; 
                                            formal_of "offset" i32_t; formal_of "whence" i32_t]);
        reserved_stub "exit"    (void_t) ([formal_of "status" i32_t]);
        reserved_stub "getchar" (i32_t)  ([]);
        reserved_stub "input"   (str_t)  ([]);
    ] in
    reserved

let expr_to_sexpr e env = match e with
    IntLit i        -> (SIntLit(i), env)

let get_type_from_sexpr = function
    SIntLit(_)      -> Datatype(Int_t)

(* Statement to SStatement Conversion *)
let rec check_sblock sl env = match sl with
    [] ->   (SBlock([SExpr(SNoexpr, Datatype(Unit_t))]), env)
  | _ ->    let (sl,_) = convert_stmt_list_to_sstmt_list sl env 
            in
            (SBlock(sl), env)

and check_expr_stmt e env =
    let se, env = expr_to_sexpr e env in
    let data_t = get_type_from_sexpr se in
    (SExpr(se, data_t), env)

and parse_stmt env = function
    Block sl                -> check_sblock sl env
  | Expr e                  -> check_expr_stmt e env
(*
  | Return e                -> check_return e env
  | If(e, s1, s2)           -> check_if e s1 s2 env
  | For(e1, e2, e3, e4)     -> check_for e1 e2 e3 e4 env
  | While(e, s)             -> check_while e s env
  | Local(s, d, e)          -> local_handler s d e env
*)

(* Semantically check a list of stmts *)
and convert_stmt_list_to_sstmt_list sl env =
    let env_ref = ref(env) in
    let rec iter = function
        head :: tail ->
            let (a_head, env) = parse_stmt !env_ref head in
            env_ref := env;
            a_head :: (iter tail)
      | [] -> []
    in
    let sstmt_list = ((iter sl), !env_ref) in
    sstmt_list

(* Generate a String Map of all classes to be used for semantic checking *)
let build_class_maps reserved cdecls =
    let reserved_map = List.fold_left  (fun m f -> StringMap.add f.sfname f m) StringMap.empty reserved
    in
    (* Check each constituent of a class: fields, member functions, constructors *)
    let helper m (cdecl : Ast.cdecl) =
        (* Check Fields *)
        let check_fields = (fun m -> (function Field(scope, s, data_t) -> 
            if (StringMap.mem (s) m) then raise (E.DuplicateField(s))
            else (StringMap.add s (Field(scope, s, data_t)) m)))
        in
        (* Check Methods *)
        let method_name = get_method_name cdecl.cname in
        let check_methods m fdecl =
            if (StringMap.mem (method_name fdecl) m) 
                then raise (E.DuplicateFunctionName(method_name fdecl))
            else if (StringMap.mem fdecl.fname reserved_map)
                then raise (E.FunctionNameReserved(fdecl.fname))
            else (StringMap.add (method_name fdecl) fdecl m)
        in
        (* TODO: Check Constructors *)
        if (StringMap.mem cdecl.cname m) then raise (E.DuplicateClassName(cdecl.cname))
        (* Add the class object to the map *)
        else StringMap.add cdecl.cname ({
            field_map = List.fold_left check_fields StringMap.empty cdecl.cbody.fields;
            method_map = List.fold_left check_methods StringMap.empty cdecl.cbody.methods;
            reserved_map = reserved_map;
            cdecl = cdecl
        }) m
    in
    List.fold_left helper StringMap.empty cdecls

(* Generate List of all functions to be used for semantic checking *)
(*
let build_function_maps reserved fdecls =
    let reserved_map = List.fold_left (fun m f -> StringMap.add f.sfname f m) StringMap.empty reserved
    in
    (* Check each constituent of a function: locals, member fucntions *)
    let helper m (cdecl : Ast.cdecl) =
        let check_fields = (fun m -> (function Field(scope, s, data_t) -> 
            if (StringMap.mem (s) m) then raise (E.DuplicateField(s))
            else (StringMap.add s (Field(scope, s, data_t)) m)))
        in
        if (StringMap.mem fdecl.fname m) then raise (E.DuplicateFunctionName(fdecl.fname))
        else StringMap.add fdecl.fname ({
            field_map = List.fold_left check_fields StringMap.empty cdecl.cbody.fields;
            reserved_map = reserved_map;
            cdecl = cdecl
        }) m
    in
    List.fold_left helper StringMap.empty fdecls
*)

(* Convert fdecls to sfdecls *)
(* Notes: *)
    (* Methods: *)
        (* Name = <root_class>.<fname> *)
        (* Prepend instance of class to function parameters *)
    (* Pass class_maps = StringMap.emtpy & cname = "" for non-method functions *)
let convert_fdecl_to_sfdecl reserved class_maps class_map cname fdecl =
    let root_cname = match fdecl.root_cname with
        Some(c) -> c
      | None -> cname
    in
    (* The class that the function takes as an additional formal *)
    let class_formal =
        if fdecl.overrides then
            Ast.Formal("this", Datatype(Objecttype(root_cname)))
        else
            Ast.Formal("this", Datatype(Objecttype(cname)))
    in
    let env_param_helper m formal = match formal with
        Formal(s, data_t) -> (StringMap.add s formal m)
      | _ -> m
    in
    let env_params = List.fold_left env_param_helper StringMap.empty (class_formal :: fdecl.formals) in
    let env = {
        env_class_maps  = class_maps;
        env_name        = cname;
        env_cmap        = class_map;
        env_locals      = StringMap.empty;
        env_parameters  = env_params;
        env_return_t    = fdecl.return_t;
        env_in_for      = false;
        env_in_while    = false;
        env_reserved    = reserved;
    }
    in
    (* Assign fname to <fname> or <class>.<fname> appropriately *)
    let fname = 
        if (cname = "")
        then fdecl.fname
        else get_method_name cname fdecl
    in
    (* Prepend the class as the first parameter to the function if it is a method *)
    let fdecl_formals =
        if not (cname = "") then
            class_formal :: fdecl.formals
        else
            fdecl.formals
    in
    let (fbody, _) = convert_stmt_list_to_sstmt_list fdecl.body env
    in
    (* Check the stmts in the fbody *)
    (*
    ignore(check_fbody fname fbody fdecl.return_t);
    let fbody =
        if (fname = "main") 
        then append_code_to_main fbody cname (Datatype(Objecttype(cname)))
        else fbody
    in
    *)
    {
        sfname      = fname;
        sreturn_t   = fdecl.return_t;
        sformals    = fdecl_formals;
        sbody       = fbody;
        fgroup      = Sast.User;
        overrides   = fdecl.overrides;
        source      = cname;
    }

(* Convert cdecls to scdecls *)
let convert_cdecl_to_scdecl sfdecls (c:Ast.cdecl) =
    {
        scname = c.cname;
        sfields = c.cbody.fields;
        sfdecls = sfdecls;
    }

(* Convert AST to SAST *)
let convert_ast_to_sast reserved class_maps (cdecls:Ast.cdecl list) =
    let is_main = 
        (fun f -> match f.sfname with s -> s = "main") 
    in
    let get_main fdecls =
        let mains = List.find_all is_main fdecls 
        in
        if List.length mains < 1 then
            raise E.MissingMainFunction
        else if List.length mains > 1 then
            raise E.MultipleMainFunctions
        else 
            List.hd mains
    in
    let remove_main fdecls =
        List.filter (fun f -> not (is_main f)) fdecls
    in
    (* TODO: Find default constructor *)
    let handle_cdecl cdecl =
        let class_map = StringMap.find cdecl.cname class_maps in
        let sfdecls = List.fold_left 
            (fun l f -> (convert_fdecl_to_sfdecl reserved class_maps class_map cdecl.cname f) :: l) 
            [] cdecl.cbody.methods 
        in
        let sfdecls = remove_main sfdecls 
        in
        let scdecl = convert_cdecl_to_scdecl sfdecls cdecl in
        (scdecl, sfdecls)
    in
    (* Generate a tuple of all semantically checked cdecls and fdecls *)
    let iter_cdecls t c =
        let scdecl = handle_cdecl c in
        (fst scdecl :: fst t, snd scdecl @ snd t)
    in
    let (scdecl_list, sfdecl_list) = List.fold_left iter_cdecls ([], []) cdecls in
    let main = get_main sfdecl_list
    in
    let fdecls = remove_main sfdecl_list
    in
    {
        classes     = scdecl_list;
        fdecls      = fdecls;
        main        = main;
        reserved    = reserved;
    }

(* Analyze *)
let analyze filename ast = match ast with
    Program(includes, specs, cdecls, fdecls) ->
        (* Include code from external files *)

        (* Add built-in functions from LLVM *)
        let reserved = add_reserved_functions 
        in
        
        (* Generate Function, Spec, Class Maps for lookup in checking functions *)
        let class_maps = build_class_maps reserved cdecls
        in
        (*
        let function_maps = build_function_maps reserved fdecls
        in
        *)
        let sast = convert_ast_to_sast reserved class_maps cdecls 
        in
        sast
