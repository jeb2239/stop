open Core.Std

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not

type primitive = 
    Int_t 
  | Float_t 
  | Unit_t 
  | Bool_t 
  | Char_t 
   


type datatype = Datatype of primitive 
            | Arraytype of datatype *int 
            | Functiontype of datatype list * datatype
            | Objecttype of string

type formal = Formal of  string* datatype
type field = Field of string* datatype



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
  | Call of string * expr list
  | Objectcreate of string * expr list
  | FuncLit of  formal list * datatype * stmt list (*the idea is that this returns a function literal
                                                (* when someone declares a function they immidialy assign
                                                it to a name*)*)
  | Arraycreate of datatype * expr list   
  | Arrayaccess of expr * expr list                                          
  | Noexpr

  and stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Static_init of string * datatype * expr


type method_dec = Method of  string * formal list * datatype * stmt list | Constructor of formal list * datatype * stmt list

type cbody = {

  fields : field list;
  constructors : method_dec list;
  methods : method_dec list;
  
}

type class_decl = {
  cname : string;
  (* extends : extends; *)
  cbody: cbody;
}

let rec type_list_of_formal formal_lis = List.map ~f:(function Formal(a,b) -> b) formal_lis



let type_of_lit = function 
  IntLit(_) -> Datatype(Int_t)
  |BoolLit(_) -> Datatype(Bool_t)
  (* |CharLit (_) -> Datatype(Char_t) *)
  |FuncLit (flist,dt,_) -> Functiontype((type_list_of_formal flist),dt)



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

let rec print_brackets = function
    1 -> "[]"
   | i -> "[]" ^ print_brackets (i-1)

let string_of_primitive = function
    Int_t -> "Int"
    | Float_t -> "Float"
    | Unit_t -> "Unit"
    | Bool_t -> "Bool"
    | Char_t -> "Char"
    (* | Object_t(s) -> "class" ^ s *)
 
let rec string_of_datatype = function
    Datatype(p) -> string_of_primitive p
    | Arraytype(p,i) -> string_of_datatype p ^ print_brackets i
    | Functiontype(args,ret) ->
     "Fun(" ^ (String.concat ~sep:","
      (List.map ~f:(string_of_datatype) args)) ^")->" ^ string_of_datatype ret
    | Objecttype(s) ->  s 

let string_of_formal = function
    Formal( s,data_t) -> s ^ ":" ^ string_of_datatype data_t 

let rec string_of_expr = function
     IntLit(i) -> string_of_int i
    | FloatLit(f) -> Pervasives.string_of_float f
    | Id(r) -> r
    | BoolLit(true) -> "true"
    | BoolLit(false) -> "false"
    | Binop(e1,o,e2) -> string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2 
    | Unop(o,e) -> string_of_uop o ^ string_of_expr e 
    (* | Vdec(e,dt) -> "var " ^  e ^ ":" ^ string_of_dtype dt ^ ";" *)
    | Assign(id,v) -> string_of_expr id ^ " = " ^ string_of_expr v ^";"
    | FuncLit(a,b,c)-> 
    (String.concat ~sep:","  
      (List.map ~f:string_of_formal a)) ^ (string_of_datatype b) ^  (string_of_stmt (Block c))
    (* (String.concat ~sep:"" (List.map ~f:string_of_stmt c)) *)
    (*this needs to be moved, to a string_of_vdecl function but for now..*)
    | Noexpr -> ""
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
    (* | Vdec(e,dt)-> "var" ^ " " ^ e^":" ^ string_of_dtype dt *)
    | Static_init(a,b,c) -> a ^" "^ string_of_datatype b ^ " "^string_of_expr c

let string_of_field = function Field(str,dt) -> "var " ^ str ^":" ^ string_of_datatype dt ^";\n"

let map_and_concat fn sep item = String.concat ~sep:sep (List.map ~f:fn item)

let string_of_method_dec = function
  Method(name,forms,dt,stmts) -> "method "^name^"=("^ (map_and_concat string_of_formal "," forms) ^":"^ string_of_datatype dt ^ string_of_stmt (Block stmts) 
  |Constructor(pattern,dt1,stmts1) -> "pattern =("^ String.concat ~sep:"," (List.map ~f:string_of_formal pattern) ^"):"^
                                      string_of_datatype dt1 ^ string_of_stmt (Block stmts1)

let rec string_of_cdecl cdec = "class " ^ cdec.cname ^ "{\n" ^ String.concat (List.map ~f:string_of_field cdec.cbody.fields)^"\n"
                                ^ map_and_concat string_of_method_dec "\n" cdec.cbody.constructors ^ map_and_concat string_of_method_dec "\n" cdec.cbody.methods
                                 ^ "\n}"

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


type program = Program of stmt list * class_decl list

let string_of_program = function Program(stmts,clas) -> map_and_concat string_of_cdecl "\n" clas ^"\n" ^String.concat ~sep:"\n" (List.map ~f:string_of_stmt stmts)

        