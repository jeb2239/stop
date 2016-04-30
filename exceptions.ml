(* Stop Exceptoins *)
exception InvalidOption of string 
exception InvalidArgc
exception NoFileArgument

(* Scanner Exceptions *)
exception IllegalCharacter of string * string * int

(* Generator Exceptions *)
exception MissingEOF

(* Semant Exceptions *)
exception ThisUsedOutsideClass
exception MissingMainFunction
exception MultipleMainFunctions
exception InvalidUnaryOperation
exception InvalidBinaryOperation
exception UndefinedID of string
exception DuplicateField of string
exception DuplicateClassName of string
exception DuplicateLocal of string
exception DuplicateFunctionName of string
exception FunctionNameReserved of string
exception ReturnTypeMismatch of string * string * string option
exception AssignmentTypeMismatch of string * string

(* Utils Exceptions *)
exception UtilsError of string

(* Codegen Exceptions *)
exception InvalidStructType of string
exception PrintfFirstArgNotString
exception PrintfMissingArgs
exception NotImplemented

exception FloatOpNotSupported
exception IntOpNotSupported
exception InvalidBinopEvaluationType

exception InvalidStructType of string
exception InvalidDatatype of string
exception LLVMFunctionNotFound of string
exception FunctionWithoutBasicBlock of string
