(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not
type primitive = Int_t | Float_t | Unit_t | Bool_t | Char_t | Class_t of string | Spec_t of string

type dtype = Arraytype of primitive * int | Dtype of primitive | Functiontype of formal_param list * dtype | Any 
type visibility = Pub | Priv
type formal_param = Formal of dtype * string | Many of dtype


 

type requirements = Method_Req of method_decl | Type_Req of dtype

type expr = 
    IntLit of int
  | FloatLit of float
  | BoolLit of bool
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
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
type program =  Program of include_stmt list * spec_decl list * cdecl list
type field = Field of visibility * dtype * string (*string is the var name*)
type spec_decl = {
  sname : string;
  body: sbody;
}

type sbody ={
   methods: method_decl list;
  typereq: dtype list;
}

type method_decl = {
  
  visibility: visibility;
  name:string;
  returnType: dtype;
  formal_param : formal_param list option;
  body : stmt list option; (*there may be not implementation if in a spec dec*)
  
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
  formal_param : formal_param list option;
  body : stmt list option; (*there may be not implementation if in a spec dec*)

}

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

let string_of_program = function
    Program(includes, stmts) -> 
        String.concat "" (List.map string_of_include includes) ^ "\n" ^
        String.concat "" (List.map string_of_stmt stmts) ^ "\n"
