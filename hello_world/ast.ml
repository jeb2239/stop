open Core.Std

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not

type dtype = 
    Int_t 
  | Float_t 
  | Unit_t 
  | Bool_t 
  | Char_t 
  | Fun_t of dtype list * dtype

 type param = Param of string * dtype  


(*	
  | Name_t of string
	| Functiontype of dtype list * dtype 
	| Arraytype of dtype * int
*)

(*
type formal_param = Formal of dtype * string 
*)
(* Class_t of string | Spec_t of string  *)

type expr = 
    IntLit of int
  | FloatLit of float
  | BoolLit of bool
  | Id of string 
  | Binop of expr * op * expr
  | Unop of uop * expr
(* I am calling this an expr for now but it will be renamed into a vdecl type*)
 (*| Vdec of string * dtype  *)
  | Assign of expr * expr
(* type vdecl =  *)
  | FuncLit of  param list * dtype * stmt list (*the idea is that this returns a function literal
                                                (* when someone declares a function they immidialy assign
                                                it to a name*)*)
  | Noexpr

  and stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Vdec of string * dtype





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

 

let rec string_of_dtype = function
    Int_t -> "Int"
    | Float_t -> "Float"
    | Unit_t -> "Unit"
    | Bool_t -> "Bool"
    | Char_t -> "Char"
    | Fun_t(args,ret) -> "Fun(" ^ (List.fold ~init:"" ~f:(fun x y -> x ^ "," ^ y) (List.map ~f:(string_of_dtype) args)) ^ ")->" ^ string_of_dtype ret

(* let string_of_param x = match x with Param(tid,t) ->  *)


let rec string_of_expr = function
     IntLit(i) -> string_of_int i
    | FloatLit(f) -> Pervasives.string_of_float f
    | Id(r) -> r
    | BoolLit(true) -> "true"
    | BoolLit(false) -> "false"
    | Binop(e1,o,e2) -> string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2 ^ ";"
    | Unop(o,e) -> string_of_uop o ^ " " ^ string_of_expr e ^ ";"
    (* | Vdec(e,dt) -> "var " ^  e ^ ":" ^ string_of_dtype dt ^ ";" *)
    | Assign(id,v) -> string_of_expr id ^ " = " ^ string_of_expr v ^";"
    | _ -> "we got an fdecl"
    (*this needs to be moved, to a string_of_vdecl function but for now..*)
    and string_of_stmt = function
     Block(stmts) ->
      "{\n" ^ (String.concat ~sep:" " (List.map ~f:string_of_stmt stmts)) ^ "}\n"
    | Expr(expr) -> string_of_expr expr ^ ";\n";
    | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
    | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
    | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
    | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
    | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
    | Vdec(e,dt)-> "var" ^ " " ^ e^":" ^ string_of_dtype dt

(*let rec string_of_dtype = function
	Int_t -> "Int"
	| Float_t-> "Float"
	| Unit_t -> "Unit"
	| Bool_t -> "Bool"
	| Name_t(a) -> a
	| Functiontype(formlist,dt) -> "not implemented"						
	| Arraytype(dt,a) -> (string_of_dtype dt) ^ "[" ^ (string_of_int a) ^ "]"
	


let rec string_of_expr = function
  | IntLit(a) -> string_of_int a
  | FloatLit(a) -> string_of_float a
  | FuncLit(fparam,dt,sl) -> "not implemented"
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  |  Block(expr) ->
      "{\n" ^ String.concat "" (List.map string_of_expr expr) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_expr s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_expr s1 ^ "else\n" ^ string_of_expr s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_expr s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_expr s

(* let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Void -> "void"
 *)
*)


type program = Program of stmt list