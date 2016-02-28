(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq
                | And | Or

type expr = 
    Literal of int
  | BoolLit of bool
  | Binop of expr * op * expr
  | Noexpr

type program = expr

(* Pretty-printing Functions *)

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

let rec string_of_expr = function
    Binop(e1, op, e2) ->
        string_of_expr e1 ^ " " ^ string_of_op op ^ " " ^ string_of_expr e2

let string_of_program expr =
    string_of_expr expr
