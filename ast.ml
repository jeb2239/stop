(* Abstract Syntax Tree and functions for printing it *)
open Core.Std

type op = Add | Sub | Mult | Div | And | Or |
          Equal | Neq | Less | Leq | Greater | Geq 
type uop = Neg | Not
type primitive = Int_t | Float_t | Bool_t | Char_t | Unit_t 

(* i.e. Arraytype (a, 2) <=> a[][]; (a, 3) <=> a[][][] *)
type datatype = 
    Datatype of primitive 
  | Arraytype of datatype * int
  | Objecttype of string

type fname = FName of string
type formal = Formal of datatype * string

type expr = 
   
    IntLit of int
  | FloatLit of float
  | BoolLit of bool
  | CharLit of char
  | StringLit of string
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Call of string * expr list
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

(* Functions *)
(* --------- *)

type fdecl = {
    fname : fname;
    return_t : datatype;
    formals : formal list;
    body : stmt list;
}

(* Classes *)
(* ------- *)

type extends = NoParent | Parent of string
type method_dec = Method of  string * formal list * datatype * stmt list | Constructor of formal list * datatype * stmt list

type cbody = {
    fields : field list;
    constructors : method_dec list;
    methods : method_dec list;
}

type class_decl = {
    cname : string;
    (*extends : extends;*)
    cbody: cbody;
}

(* Program Definition *)
(* ------------------ *)

type program =  Program of include_stmt list * stmt list * class_decl list

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
    Int_t -> "int"
  | Float_t -> "float"
  | Bool_t -> "bool"
  | Char_t -> "char"
  | Unit_t -> "Unit"

let rec print_brackets = function
    1 -> "[]"
  | i -> "[]" ^ print_brackets (i - 1)

let rec string_of_datatype = function
    Datatype(p) -> string_of_primitive p
  | Arraytype(p, i) -> string_of_datatype p ^ print_brackets i
  | Objecttype(s) -> "class" ^ s

(* type fname = FName of string *)
let string_of_fname = function
    FName(s) -> s

(* type formal = Formal of datatype * string *)
let string_of_formal = function
    Formal(data_t, s) -> s ^ ":" ^ string_of_datatype data_t 

let rec string_of_expr = function
   
  |  IntLit(i) -> string_of_int i
  | FloatLit(f) -> string_of_float f
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | CharLit(c) -> String.make 1 c
  | StringLit(s) -> "\"" ^ s ^ "\""
  | Id(i) -> i
  | Binop(e1, op, e2) ->
        string_of_expr e1 ^ " " ^ string_of_op op ^ " " ^ string_of_expr e2
  | Unop(op, e1) ->
        string_of_uop op ^ " " ^ string_of_expr e1
  | Call(s, e_list) -> s ^ "(" ^ String.concat ~sep:", " (List.map ~f:string_of_expr e_list) ^ ")"
  | Noexpr -> ""
  
let rec string_of_stmt = function
    Block(stmts) ->
        "{\n" ^ String.concat ~sep:"" (List.map ~f:string_of_stmt stmts) ^ "}\n"
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

let string_of_cdecl cdecl =(* match cdecl.extends with*)
    (* NoParent -> *)
        "class " ^ cdecl.cname ^ " { }\n"
    (* | Parent(s) -> *)
        (* "class " ^ cdecl.cname ^ " extends " ^ s ^ " { }\n" *)

let string_of_fdecl fdecl =
    "function" ^ " " ^ string_of_fname fdecl.fname ^ " = (" ^
    String.concat ~sep:", " (List.map ~f:string_of_formal fdecl.formals) ^
    "):" ^ string_of_datatype fdecl.return_t ^ "{\n" ^ 
    String.concat ~sep:"" (List.map ~f:string_of_stmt fdecl.body) ^
    "}\n"

let string_of_program = function
    Program(includes, fdecls,cdecls) -> 
        String.concat ~sep:"" (List.map ~f:string_of_include includes) ^ "\n" ^
        String.concat ~sep:"" (List.map ~f:string_of_stmt fdecls) ^ "\n" ^
        String.concat ~sep:"" (List.map ~f:string_of_cdecl cdecls)

