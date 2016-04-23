(* Stop Compiler Top Level *)

open Core.Std 

module E = Exceptions
module L = Llvm
module G = Generator
module P = Parser
module S = Scanner
module A = Analysis

type action = Tokens | Print | Ast | Sast | Compile_to_stdout | Compile_to_file | Help

let get_action = function
    "-t"    -> Tokens
  | "-p"    -> Print
  | "-a"    -> Ast
  | "-s"    -> Sast
  | "-l"    -> Compile_to_stdout
  | "-c"    -> Compile_to_file
  | "-h"    -> Help
  | _ as s  -> raise (E.InvalidOption s)

let check_single_argument = function
        "-h"    -> Help, ""
    |   "-tendl"
    |   "-t"
    |   "-p"
    |   "-ast"
    |   "-sast"
    |   "-c"
    |   "-f"    -> raise (Exceptions.NoFileArgument)
    |   _ as s  -> Compile_to_file, s

let help_string = (
      "Usage: stop [-option] <source file>\n" ^
        "-option: (defaults to \"-c\")\n" ^
        "\t-t: Print tokens\n" ^
        "\t-p: Prints AST as program\n" ^
        "\t-a: Prints AST as json\n" ^
        "\t-s: Prints SAST as json\n" ^
        "\t-l: Compiles source to stdout\n" ^
        "\t-c: Compiles source to file (<filename>.<ext> -> <filename>.ll)\n" ^
        "\t-h: Print help\n" 
    )

let stop_name filename =
    let basename = Filename.basename filename in
    let filename = Filename.chop_extension basename in
    filename ^ ".ll"

let _ = 
    ignore(Printexc.record_backtrace true);
    try
        (* Get the Appropriate Action *)
        let (action, filename) = 
            if Array.length Sys.argv = 1 then
                Help, ""
            else if Array.length Sys.argv = 2 then
                check_single_argument (Sys.argv.(1))
            else if Array.length Sys.argv = 3 then
                get_action Sys.argv.(1), Sys.argv.(2)
            else raise E.InvalidArgc (Array.length Sys.argv)
        in 

        (* Iterative Application of each Compiler "Stage" *)
        let file_in = open_in filename in 
        let lexbuf = Lexing.from_channel file_in in
        let token_list = G.build_token_list filename lexbuf in
        let ast = G.build_ast filename token_list in

        (* Respond Appropriately to Action *)
        match action with
            Tokens              -> print_string (Utils.token_list_to_string token_list)
          | Print               -> print_string (Utils.string_of_program ast)
          | Ast                 -> print_string "Not Implemented\n"
          | Sast                -> print_string "Not Implemented\n"
          | Compile_to_stdout   -> print_string "Not Implemented\n"
          | Compile_to_file     -> print_string "Not Implemented\n"
          | Help                -> print_string help_string
    with 
        (* Deal with Exceptions *)
        E.IllegalCharacter(file, c, ln) -> 
            print_string ("Illegal character '" ^ c ^ "' in line " ^ 
                            string_of_int ln ^ " of " ^ file ^ "\n")
      | _ as e -> raise e

(*
            Compile in
        let lexbuf = Lexing.from_channel stdin in 
        let ast = Parser.program Scanner.token lexbuf in
        Semant.check ast;
        match action with
          | LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.translate ast))
          | Compile -> let m = Codegen.translate ast in
          Llvm_analysis.assert_valid_module m;
          print_string (Llvm.string_of_llmodule m)
*)
