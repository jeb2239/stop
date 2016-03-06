(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or
type primitive = Int_t | Float_t | Void_t | Bool_t | Char_t | Objecttype of string | ConstructorType | Null_t
type datatype = Arraytype of primitive * int | Datatype of primitive | Any
type uop = Neg | Not
type extends = NoParent | Parent of string
type fname = Constructor | FName of string
type formal = Formal of datatype * string | Many of datatype
type field = Field of scope * datatype * string
type expr = 
    IntLit of int
  | FloatLit of float
  | BoolLit of bool
  | StringLit of string
  | CharLit of char
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Noexpr
  | Assign of expr * expr
  | ArrayCreate of datatype * expr list
  | ArrayAccess of expr * expr list
  |   ObjAccess of expr * expr
  |   Call of string * expr list
  |   ObjectCreate of string * expr list
  |   ArrayPrimitive of expr list

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
   

type include_stmt = Include of string

(*type formal = Formal of datatype * string | Many of datatype*)
(* type program = include_stmt list * class_decl list *)
type program =  Program of include_stmt list * stmt list

type func_decl = {
  fname : fname;
  returnType : datatype;
  formals : formal list;
  body : stmt list;
  overrides : bool;
  root_cname : string option;
}

type cbody = {
  fields : field list;
  constructors : func_decl list;
  methods : func_decl list;
}

type class_decl = {
  cname : string;
  extends : extends;
  cbody: cbody;
  match_list: formal list
}


(*type func_decl = {
  (* scope : scope; *)
  fname : fname;
  returnType : datatype;
  formals : formal list;
  body : stmt list;
  overrides : bool;
  root_cname : string option;
}*)
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
