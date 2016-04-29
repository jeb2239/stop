(* Semantic Analyzer for Stop Language *)

open Core.Std
open Ast
open Sast

module G = Generator
module U = Utils
module E = Exceptions

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

(* General String of List Function *)
let string_of_list string_of_item l = 
    "[" ^ String.concat ~sep:", " (List.map ~f:string_of_item l) ^ "]"

(* Record which contains information re: Classes *)
type class_map = {
    field_map       : Ast.field StringMap.t;
    method_map      : Ast.fdecl StringMap.t;
    (* constructor_map : Ast.fdecl StringMap.t; *)
    reserved_map    : Sast.sfdecl StringMap.t;
    cdecl           : Ast.cdecl;
}

(* Analysis Environment for functions *)
type env = {
    env_name        : string;
    env_fname       : string option;
    env_class_maps  : class_map StringMap.t option;
    env_cmap        : class_map option;
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
    env_fname       = env.env_fname;
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
    env_fname       = env.env_fname;
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

(* TODO: Add actual function types *)
let add_reserved_functions =
    let reserved_stub fname return_t formals =
        {
            sfname      = fname;
            sreturn_t   = return_t;
            sformals    = formals;
            sbody       = [];
            fgroup      = Sast.Reserved;
            overrides   = false;
            source      = None;
            sftype      = Functiontype([], Datatype(Unit_t))
        }
    in
    let i32_t = Datatype(Int_t) in
    let void_t = Datatype(Unit_t) in
    let str_t = Arraytype(Char_t, 1) in
    let formal_of s data_t = Formal(s, data_t) in
    let reserved = [
        reserved_stub "printf" (void_t) 
            ([Many(Any)]);

        reserved_stub "malloc" (str_t)  
            ([formal_of "size" i32_t ]);

        reserved_stub "cast" (Any) 
            ([formal_of "in" Any]);

        reserved_stub "sizeof" (i32_t)
            ([formal_of "in" Any]);

        reserved_stub "open" (i32_t)
            ([formal_of "path" str_t; formal_of "flags" i32_t]);

        reserved_stub "close" (i32_t)  
            ([formal_of "fd" i32_t]);

        reserved_stub "read" (i32_t)  
            ([formal_of "fd" i32_t; formal_of "buf" str_t; formal_of "nbyte" i32_t]);

        reserved_stub "write" (i32_t)  
            ([formal_of "fd" i32_t; formal_of "buf" str_t; formal_of "nbyte" i32_t]);

        reserved_stub "lseek" (i32_t)  
            ([formal_of "fd" i32_t; formal_of "offset" i32_t; formal_of "whence" i32_t]);

        reserved_stub "exit" (void_t) 
            ([formal_of "status" i32_t]);

        reserved_stub "getchar" (i32_t)  
            ([]);

        reserved_stub "input" (str_t)
            ([]);
    ] in
    reserved

(* TODO: Add all match cases for stmts, exprs *)
let expr_to_sexpr e env = match e with
    IntLit i        -> (SIntLit(i), env)
  | FloatLit b      -> (SFloatLit(b), env)
  | BoolLit b       -> (SBoolLit(b), env)
  | CharLit c       -> (SCharLit(c), env)
  | StringLit s     -> (SStringLit(s), env)
  (* | Id s            -> (SID(s, get_ID_type env s), env) *)

let get_type_from_sexpr = function
    SIntLit(_)                  -> Datatype(Int_t)
  | SFloatLit(_)                -> Datatype(Float_t)
  | SBoolLit(_)                 -> Datatype(Bool_t)
  | SCharLit(_)                 -> Datatype(Char_t)
  | SStringLit(_)               -> Arraytype(Char_t, 1)
  | SId(_, data_t)              -> data_t
  | SBinop(_, _, _, data_t)     -> data_t
  | SUnop(_, _, data_t)         -> data_t
  | SCall(_, _, data_t, _)      -> data_t
  | SObjAccess(_, _, data_t)    -> data_t
  | SNoexpr                     -> Datatype(Unit_t)

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

and check_return e env =
    let (se, _) = expr_to_sexpr e env in
    let data_t = get_type_from_sexpr se in
    match data_t, env.env_return_t  with 
        (* Allow unit returns for reference types e.g. objects, arrays *)
        (* TODO: See if this makes sense for Unit_t... *)
        Datatype(Unit_t), Datatype(Object_t(_))
      | Datatype(Unit_t), Arraytype(_, _) -> (SReturn(se, data_t), env)
      | _ -> 
            if data_t = env.env_return_t
            then (SReturn(se, data_t), env)
            else raise (E.ReturnTypeMismatch
                (U.string_of_datatype data_t, 
                U.string_of_datatype env.env_return_t, 
                env.env_fname))
            

and parse_stmt env = function
    Block sl                -> check_sblock sl env
  | Expr e                  -> check_expr_stmt e env
  | Return e                -> check_return e env
  (*
  | If(e, s1, s2)           -> check_if e s1 s2 env
  | For(e1, e2, e3, e4)     -> check_for e1 e2 e3 e4 env
  | While(e, s)             -> check_while e s env
  | Local(s, d, e)          -> local_handler s d e env 
  *)

(* Semantically check a list of stmts; Convert to sstmts *)
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
    let reserved_map = 
        List.fold_left reserved
            ~init:StringMap.empty 
            ~f:(fun m f -> StringMap.add m ~key:f.sfname ~data:f)
    in
    (* Check each constituent of a class: fields, member functions, constructors *)
    let helper m (cdecl : Ast.cdecl) =
        (* Check Fields *)
        let check_fields = (fun m -> (function Field(scope, s, data_t) -> 
            if (StringMap.mem m (s)) then raise (E.DuplicateField(s))
            else (StringMap.add m ~key:s ~data:(Field(scope, s, data_t)))))
        in
        (* Check Methods *)
        let method_name = get_method_name cdecl.cname in
        let check_methods m fdecl =
            if (StringMap.mem m (method_name fdecl)) 
                then raise (E.DuplicateFunctionName(method_name fdecl))
            else if (StringMap.mem reserved_map fdecl.fname)
                then raise (E.FunctionNameReserved(fdecl.fname))
            else (StringMap.add m ~key:(method_name fdecl) ~data:fdecl)
        in
        (* TODO: Check Constructors *)
        if (StringMap.mem m cdecl.cname) then raise (E.DuplicateClassName(cdecl.cname))
        (* Add the class object to the map *)
        else StringMap.add m
            ~key:cdecl.cname 
            ~data:({
                field_map = List.fold_left cdecl.cbody.fields
                    ~f:check_fields 
                    ~init:StringMap.empty;
                method_map = List.fold_left cdecl.cbody.methods
                    ~f:check_methods 
                    ~init:StringMap.empty;
                reserved_map = reserved_map;
                cdecl = cdecl
            }) 
    in
    List.fold_left cdecls
        ~f:helper 
        ~init:StringMap.empty 

(* Generate List of all functions to be used for semantic checking *)
let build_function_maps reserved fdecls =
    let reserved_map = 
        List.fold_left reserved
            ~f:(fun m f -> StringMap.add m ~key:f.sfname ~data:f) 
            ~init:StringMap.empty
    in
    (* Check each function *)
    let check_functions m fdecl =
        if (StringMap.mem m (fdecl.fname)) 
            then raise (E.DuplicateFunctionName(fdecl.fname))
        else if (StringMap.mem reserved_map fdecl.fname )
            then raise (E.FunctionNameReserved(fdecl.fname))
        else (StringMap.add m ~key:(fdecl.fname) ~data:fdecl)
    in
    List.fold_left  fdecls 
        ~f:check_functions 
        ~init:StringMap.empty

(* Convert a method to a semantically checked function *)
(* Name = <root_class>.<fname> *)
(* Prepend instance of class to function parameters *)
let convert_method_to_sfdecl reserved class_maps cname fdecl =
    let class_map = StringMap.find_exn class_maps cname 
    in
    let root_cname = match fdecl.root_cname with
        Some(c) -> c
      | None -> cname
    in
    (* The class that the function takes as an additional formal *)
    let class_formal =
        if fdecl.overrides then
            Ast.Formal("this", Datatype(Object_t(root_cname)))
        else
            Ast.Formal("this", Datatype(Object_t(cname)))
    in
    let env_param_helper m formal = match formal with
        Formal(s, data_t) -> (StringMap.add m ~key:s ~data:formal)
      | _ -> m
    in
    let env_params = List.fold_left (class_formal :: fdecl.formals)
        ~f:env_param_helper 
        ~init:StringMap.empty 
    in
    let env = {
        env_name        = cname;
        env_fname       = None;
        env_class_maps  = Some(class_maps);
        env_cmap        = Some(class_map);
        env_locals      = StringMap.empty;
        env_parameters  = env_params;
        env_return_t    = fdecl.return_t;
        env_in_for      = false;
        env_in_while    = false;
        env_reserved    = reserved;
    }
    in
    (* Assign fname to <fname> or <class>.<fname> appropriately *)
    let fname = get_method_name cname fdecl
    in
    (* Prepend the class as the first parameter to the function if it is a method *)
    let fdecl_formals = class_formal :: fdecl.formals
    in
    (* Check the stmts in the fbody *)
    let (fbody, _) = convert_stmt_list_to_sstmt_list fdecl.body env
    in

    (* TODO: Correctly get sftype *)
    {
        sfname      = fname;
        sreturn_t   = fdecl.return_t;
        sformals    = fdecl_formals;
        sbody       = fbody;
        fgroup      = Sast.User;
        overrides   = fdecl.overrides;
        source      = Some(cname);
        sftype      = Functiontype([], fdecl.return_t);
    }

(* Convert a function to a semantically checked function *)
let convert_fdecl_to_sfdecl reserved fdecl =
    let env_param_helper m formal = match formal with
        Formal(s, data_t) -> (StringMap.add m ~key:s ~data:formal)
      | _ -> m
    in
    let env_params = List.fold_left fdecl.formals
        ~f:env_param_helper 
        ~init:StringMap.empty 
    in
    let env = {
        env_name        = fdecl.fname;
        env_fname       = None;
        env_class_maps  = None;
        env_cmap        = None;
        env_locals      = StringMap.empty;
        env_parameters  = env_params;
        env_return_t    = fdecl.return_t;
        env_in_for      = false;
        env_in_while    = false;
        env_reserved    = reserved;
    }
    in
    (* Assign fname to <fname> or <class>.<fname> appropriately *)
    let fname = fdecl.fname
    in
    (* Prepend the class as the first parameter to the function if it is a method *)
    let fdecl_formals = fdecl.formals
    in
    (* Check the stmts in the fbody *)
    let (fbody, _) = convert_stmt_list_to_sstmt_list fdecl.body env
    in
    (*
    ignore(check_fbody fname fbody fdecl.return_t);
    let fbody =
        if (fname = "main") 
        then append_code_to_main fbody cname (Datatype(Objecttype(cname)))
        else fbody
    in
    *)

    (* TODO: Correctly get sftype *)
    {
        sfname      = fname;
        sreturn_t   = fdecl.return_t;
        sformals    = fdecl_formals;
        sbody       = fbody;
        fgroup      = Sast.User;
        overrides   = fdecl.overrides;
        source      = None;
        sftype      = Functiontype([], fdecl.return_t);
    }

(* Convert cdecls to scdecls *)
let convert_cdecl_to_scdecl sfdecls (c:Ast.cdecl) =
    {
        scname = c.cname;
        sfields = c.cbody.fields;
        sfdecls = sfdecls;
    }

(* Convert AST to SAST *)
(* TODO: Add function maps *)
let convert_ast_to_sast reserved 
    class_maps (cdecls : Ast.cdecl list) 
    function_maps (fdecls : Ast.fdecl list) =

    let is_main = 
        (fun f -> match f.sfname with s -> s = "main") 
    in
    let get_main fdecls =
        let mains = (List.filter ~f:is_main fdecls)
        in
        if List.length mains < 1 then
            raise E.MissingMainFunction
        else if List.length mains > 1 then
            raise E.MultipleMainFunctions
        else 
            List.hd_exn mains
    in
    let remove_main fdecls =
        List.filter ~f:(fun f -> not (is_main f)) fdecls
    in
    (* Buid up a list of scdecls and sfdecls *)

    (* TODO: Find default constructor *)
    let handle_cdecl cdecl =
        let class_map = StringMap.find_exn class_maps cdecl.cname in
        let sfdecls = List.fold_left cdecl.cbody.methods 
            ~f:(fun l f -> (convert_method_to_sfdecl reserved class_maps cdecl.cname f) :: l)
            ~init:[] 
        in
        let sfdecls = remove_main sfdecls 
        in
        let scdecl = convert_cdecl_to_scdecl sfdecls cdecl in
        (scdecl, sfdecls)
    in
    (* Generate a tuple of all semantically checked cdecls and fdecl methods *)
    let iter_cdecls t c =
        let scdecl = handle_cdecl c in
        (fst scdecl :: fst t, snd scdecl @ snd t)
    in
    let (scdecl_list, sfdecl_list) = List.fold_left cdecls
        ~f:iter_cdecls 
        ~init:([], []) 
    in
    (* Append non-method fdecls to the tuple *)
    let sfdecls = List.fold_left fdecls
        ~f:(fun l f -> (convert_fdecl_to_sfdecl reserved f) :: l) 
        ~init:[] 
    in
    let (scdecl_list, sfdecl_list) = (scdecl_list, sfdecls @ sfdecl_list)
    in
    let main = get_main sfdecl_list
    in
    let sfdecl_list = remove_main sfdecl_list
    in
    {
        classes     = scdecl_list;
        fdecls      = sfdecl_list;
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
        let function_maps = build_function_maps reserved fdecls
        in
        let sast = convert_ast_to_sast reserved class_maps cdecls function_maps fdecls
        in
        sast
