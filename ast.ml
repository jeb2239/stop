type op = Add | Sub | Mul | Div

type expr = 
    Literal of int
  | BoolLit of bool
  | Binop of expr * op * expr
  | Noexpr
