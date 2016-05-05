(* Stop Exceptoins *)
exception InvalidOption of string 
exception InvalidArgc
exception NoFileArgument

(* Scanner Exceptions *)
exception IllegalCharacter of string * string * int

(* Parser Exceptions *)
exception CannotDefineVariableLengthArgFunction

(* Generator Exceptions *)
exception MissingEOF

(* Semant Exceptions *)
exception ThisUsedOutsideClass
exception MissingMainFunction
exception MultipleMainFunctions
exception InvalidUnaryOperation
exception UnexpectedNoexpr
exception UnexpectedType
exception InvalidBinaryOperation
exception InvalidEqualityBinop of string * string
exception UndefinedId of string
exception DuplicateField of string
exception DuplicateClassName of string
exception DuplicateVar of string
exception DuplicateFunctionName of string
exception FunctionNameReserved of string
exception ReturnTypeMismatch of string * string * string option
exception AssignmentTypeMismatch of string * string
exception LocalAssignmentTypeMismatch of string * string
exception LocalAssignmentTypeNotAssignable of string
exception ArrayAccess of string
exception UndefinedFunction of string
exception LHSofObjectAccessMustBeAccessible
exception RHSofObjectAccessMustBeAccessible
exception UnknownClassVar
exception CannotUseThisKeywordOutsideOfClass
exception InvalidIfStatementType
exception InvalidForStatementType
exception InvalidWhileStatementType

(* Utils Exceptions *)
exception UtilsError of string

(* Codegen Exceptions *)
exception FieldIndexNotFound
exception PrintfFirstArgNotString
exception PrintfMissingArgs
exception NotImplemented
exception FloatOpNotSupported
exception IntOpNotSupported
exception UnopNotSupported
exception InvalidUnopEvaluationType
exception InvalidBinopEvaluationType
exception InvalidStructType of string
exception InvalidStructType of string
exception InvalidDatatype of string
exception LLVMFunctionNotFound of string
exception FunctionWithoutBasicBlock of string
exception AssignmentLhsMustBeAssignable
