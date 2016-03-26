(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type primitive = Int | Float | Bool | Char | Unit

type datatype = Datatype of primitive

type uop = Neg | Not

type expr = 
    IntLit of int
  | FloatLit of float
  | BoolLit of bool
  | CharLit of char
  | StringLit of string
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
  | Local of datatype * string * expr

type field = Field of datatype * string
type include_stmt = Include of string

(* Classes *)

type extends = NoParent | Parent of string

type cbody = {
    fields : field list;
}

type class_decl = {
    cname : string;
    extends : extends;
    cbody: cbody;
}

(* Program Definition *)

(* type program = include_stmt list * class_decl list *)
type program =  Program of include_stmt list * class_decl list

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

let string_of_primitive = function 
    Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | Char -> "char"
  | Unit -> "Unit"

let string_of_datatype = function
    Datatype(d) -> string_of_primitive d

let rec string_of_expr = function
    IntLit(i) -> string_of_int i
  | FloatLit(f) -> string_of_float f
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | CharLit(c) -> String.make 1 c
  | StringLit(s) -> s
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
  | Local(dtype, s, e) -> ( match e with 
          Noexpr -> string_of_datatype dtype ^ " " ^ s ^ ";\n"
        | _ -> string_of_datatype dtype ^ " " ^ s ^ " = " ^ string_of_expr e ^ ";\n" )
            
let string_of_include = function
    Include(s) -> "#include \"" ^ s ^ "\"\n"

let string_of_cdecl cdecl = match cdecl.extends with
    NoParent ->
        "class " ^ cdecl.cname ^ " { }\n"
    | Parent(s) ->
        "class " ^ cdecl.cname ^ " extends " ^ s ^ " { }\n"

let string_of_program = function
    Program(includes, cdecls) ->
        String.concat "" (List.map string_of_include includes) ^ "\n" ^
        String.concat "" (List.map string_of_cdecl cdecls) ^ "\n"
