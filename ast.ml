(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not
type primitive = Int_t | Float_t | Unit_t | Bool_t | Char_t | Class_t of string | Spec_t of string 
|Functiontype of formal_param list * dtype | Arraytype of primitive * int
and dtype = Dtype of primitive | Any 
and formal_param = Formal of dtype * string | Many of dtype

(*
formal params are a mutually recursive type with dtype

--the reason this is mutually recursive is because if you look
at the different terms of dtype, you will see Function types are defined in terms of their
parameters (aka formal_param list) and their return type

but formal_param lists also depend on certain members of dtype hence they are mutually recursive
*)

type visibility = Pub | Priv



 



type expr = 
    IntLit of int
  | FloatLit of float
  | BoolLit of bool
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | FuncLit of formal_param * dtype * stmt list (*the idea is that this return a function literal*)
                                                (* when someone declares a function they immidialy assign
                                                it to a name*)
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type include_stmt = Include of string

(* type program = include_stmt list * class_decl list *)

type field = Field of visibility * dtype * string (*string is the var name*)

type method_decl = {
  
  visibility: visibility;
  name:string;
  returnType: dtype ;
  formal_param : formal_param list ;
  body : stmt list; (*there may be not implementation if in a spec dec*)
  
}

type method_spec = {

  visibility: visibility;
  name:string;
  returnType: dtype ;
  formal_param : formal_param list ;
}


type sbody ={
   methods: method_spec list;
  typereq: dtype list;
}

type spec_decl = {
  sname : string;
  body: sbody;
}





type cbody ={
  fields : field list;
  methods: method_decl list;
  pattern_constructors : method_decl list;
}

type class_decl = {
  cname : string;
  cbody : cbody;
 
}

type func_decl = {
  
  name:string;
  returnType: dtype;
 (* func_type: dtype list; (*do we actually need this?*)*)
  formal_param : formal_param list;
  body : stmt list; (*there may be not implementation if in a spec dec*)

}

type requirements = Method_Req of method_spec | Type_Req of dtype
type program =  Program of include_stmt list * spec_decl list * class_decl * stmt list
(* Pretty-printing Functions *)
(* ------------------------- *)

let string_of_op = function 
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_expr = function
    IntLit(i) -> string_of_int i
  | FloatLit(f) -> string_of_float f
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(i) -> i
  | Binop(e1, op, e2) ->
        string_of_expr e1 ^ " " ^ string_of_op op ^ " " ^ string_of_expr e2
  | Unop(op, e1) ->
        string_of_uop op ^ " " ^ string_of_expr e1
  | Noexpr -> ""
  
let rec string_of_stmt = function
    Block(stmts) ->
        "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s1 
                        ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) -> "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " 
                            ^ string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_include = function
    Include(s) -> "include \"" ^ s ^ "\"\n"

let string_of_specs = function 





let string_of_program = function
    Program(includes, specs,classes , stmts) -> 
        String.concat "" (List.map string_of_include includes) ^ "\n" 
        (*String.concat "" (List.map string_of_stmt stmts) ^ "\n"*)
