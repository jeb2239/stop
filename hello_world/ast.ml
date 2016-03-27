type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not

type dtype = 
	Int_t 
	| Float_t 
	| Unit_t 
	| Bool_t 
	| Char_t 
	| Name_t of string
	| Functiontype of formal_param list * dtype 
	| Arraytype of dtype * int
and formal_param = Formal of dtype * string | Many of dtype



(* Class_t of string | Spec_t of string  *)

type expr = 
    IntLit of int
  | FloatLit of float
  | BoolLit of bool
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | FuncLit of formal_param * dtype * stmt list (*the idea is that this returns a function literal*)
                                                (* when someone declares a function they immidialy assign
                                                it to a name*)
  | Noexpr




and stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt


type program = Program of stmt list