open Ofluxl
open Std

let () =
  match Syntax.Parse.stdin () with
  | Ok program -> 
    sexp_println Syntax.Ast.sexp_of_program program;
    println ""
  | Error err -> Syntax.Err.print err
