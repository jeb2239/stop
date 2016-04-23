(* Stop Exceptoins *)
exception InvalidOption of string 
exception InvalidArgc
exception NoFileArgument


(* Scanner Exceptions *)
exception IllegalCharacter of string * string * int

(* Generator Exceptions *)
exception MissingEOF

(* Semant Exceptions *)

(* Utils Exceptions *)
exception UtilsError of string

(* Codegen Exceptions *)
exception InvalidStructType of string
exception PrintfFirstArgNotString
exception PrintfMissingArgs
exception NotImplemented
