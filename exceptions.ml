(* Stop Exceptoins *)
exception InvalidOption of string 
exception InvalidArgc
exception NoFileArgument


(* Scanner Exceptions *)
exception IllegalCharacter of string * string * int

(* Generator Exceptions *)
exception MissingEOF

(* Semant Exceptions *)
exception DuplicateField of string
exception DuplicateClassName of string

exception DuplicateLocal of string
exception DuplicateFunctionName of string
exception FunctionNameReserved of string

exception MissingMainFunction
exception MultipleMainFunctions

(* Utils Exceptions *)
exception UtilsError of string

(* Codegen Exceptions *)
exception InvalidStructType of string
exception PrintfFirstArgNotString
exception PrintfMissingArgs
exception NotImplemented
