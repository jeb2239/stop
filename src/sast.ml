(* Semantically Checked AST *)
(* ------------------------ *)

(* Resolves datatypes in exprs, sstmt s *)

open Ast

type sexpr = 
    SIntLit of int
  | SFloatLit of float
  | SBoolLit of bool
  | SCharLit of char
  | SStringLit of string
  | SId of string * datatype
  | SUnop of uop * sexpr * datatype
  | SBinop of sexpr * op * sexpr * datatype
  | SAssign of sexpr * sexpr * datatype
  | SCall of string * sexpr list * datatype * int
  | SObjAccess of sexpr * sexpr * datatype
  | SArrayAccess of sexpr * sexpr list * datatype
  | SNoexpr

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr * datatype
  | SReturn of sexpr * datatype
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt 
  | SWhile of sexpr * sstmt 
  | SLocal of string * datatype * sexpr

type fgroup = User | Reserved

type sfdecl = {
    sfname : string;
    sreturn_t : datatype;
    sformals : formal list;
    sbody : sstmt list;
    fgroup : fgroup;
    overrides : bool;
    source : string option;
    sftype : datatype;
}

type scdecl = {
    scname : string;
    sfields : field list;
    sfdecls : sfdecl list;
}

type sprogram = {
    classes : scdecl list;
    functions : sfdecl list;
    main : sfdecl;
}
