open Ofluxl.Std
open Ofluxl.Syntax
open Ofluxl.Infer
open Ofluxl.Types

let parse cont lexbuf =
  try cont (Parser.main Lexer.token lexbuf) with
  | Lexer.Error _ -> Err.print_error @@ Err.Lexing lexbuf
  | Parser.Error -> Err.print_error @@ Err.Parsing lexbuf

let parse_stdin cont = Lexing.from_channel Stdio.stdin |> parse cont
let parse_str cont str = Lexing.from_string str |> parse cont

let print_program = sexp_println Ast.sexp_of_program
let print_env = sexp_println Env.Fixed.sexp_of_t
let print_kinds = sexp_println (Map.sexp_of_m__t (module Tvar) Kind.Fixed.sexp_of_t)

let solve program =
  println "program:";
  print_program program;
  println "";

  let env, kinds = Solve.solve_exn program in

  println "env:";
  print_env env;
  println "";

  println "kinds:";
  print_kinds kinds;
  println ""

let () =
  parse_stdin solve
