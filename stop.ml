(* (* Top-level of the Stop compiler: scan & parse the input, 
   check the resulting AST, generate LLVM IR, and dump the module *)
open Semant
type action = Ast | LLVM_IR | Compile

let _ = 
    let action = if Array.length Sys.argv > 1 then
        List.assoc Sys.argv.(1) [ ("-a", Ast);      (* Print the AST only *)
                                  ("-l", LLVM_IR);  (* Generate LLVM, don't check *)
                                  ("-c", Compile) ] Generate, check LLVM IR
    else Compile in
    let lexbuf = Lexing.from_channel stdin in 
    let ast = Parser.program Scanner.token lexbuf in
    (*Semant.check ast;*)
    match action with
        Ast -> print_string (Ast.string_of_program ast)
    (* | LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.translate ast))
      | Compile -> let m = Codegen.translate ast in
      Llvm_analysis.assert_valid_module m;
      print_string (Llvm.string_of_llmodule m)*)
 *)

 (*type action = Ast | LLVM_IR | Compile

let _ = 
    let action = if Array.length Sys.argv > 1 then
        List.assoc Sys.argv.(1) [ ("-a", Ast);      (* Print the AST only *)
                                 ("-l", LLVM_IR);   (* Generate LLVM, don't check *)
                                 ("-c", Compile) ]  (* Generate, check LLVM IR *)
    else Ast in
    let lexbuf = Lexing.from_channel stdin in 
    let ast = Parser.stmt Scanner.token lexbuf in
    match action with
        Ast -> print_string (Ast.string_of_stmt ast)
        | LLVM_IR -> print_string "Not Yet Implemented\n"
        | Compile -> print_string "Not Yet Implemented\n"
*)
open Ast
type action = Ast | LLVM_IR | Compile

let _ = 
    let action = if Array.length Sys.argv > 1 then
        List.assoc Sys.argv.(1) [ ("-a", Ast);      (* Print the AST only *)
                                  ("-l", LLVM_IR);  (* Generate LLVM, don't check *)
                                  ("-c", Compile) ] (* Generate, check LLVM IR *)
    else Compile in
    let lexbuf = Lexing.from_channel stdin in 
    let ast = Parser.program Scanner.token lexbuf in
  (* Semant.check ast;*)
    match action with
        Ast -> print_string (Ast.string_of_program ast)
      | LLVM_IR -> print_string "Not Yet Implemented" (*print_string (Llvm.string_of_llmodule (Codegen.translate ast))*)
      | Compile -> print_string "Not Yet Implemented" (*let m = Codegen.translate ast in
      Llvm_analysis.assert_valid_module m;
      print_string (Llvm.string_of_llmodule m)*)