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
  | SBinop of sexpr * op * sexpr * datatype
  | SUnop of uop * sexpr * datatype
  | SCall of string * sexpr list * datatype * int
  | SNoexpr

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr * datatype
  | SReturn of sexpr * datatype
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt 
  | SWhile of sexpr * sstmt 
  | SLocal of datatype * string * expr

type fgroup = User | Reserved

type sfdecl = {
    sfname : string;
    sreturn_t : datatype;
    sformals : formal list;
    sbody : sstmt list;
    fgroup : fgroup;
    overrides : bool;
    source : string;
}

type scdecl = {
    scname : string;
    sfields : field list;
    sfdecls : sfdecl list;
}

type sprogram = {
    classes : scdecl list;
    fdecls : sfdecl list;
    main : sfdecl option;
    reserved : sfdecl list;
}
