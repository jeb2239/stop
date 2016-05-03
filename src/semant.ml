(* Semantic checking for the MicroC compiler *)

open Ast
open Core.Std

module StringMap = Map.Make(String)

(* Semantic checking of a program. *)
(* Returns void if successful, throws an exception if something is wrong. *)

(* Check each global variable, then check each function *)

let check ast = match ast with
    Program(includes, specs, classes, functions) -> ()
