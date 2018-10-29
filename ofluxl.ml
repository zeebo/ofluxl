open Std

let parse cont lexbuf =
  try cont (Parser.main Lexer.token lexbuf) with
  | Lexer.Error _ -> Err.print_error @@ Err.Lexing lexbuf
  | Parser.Error -> Err.print_error @@ Err.Parsing lexbuf

let parse_stdin cont = Lexing.from_channel Stdio.stdin |> parse cont
let parse_str cont str = Lexing.from_string str |> parse cont

let print_program = sexp_println Ast.sexp_of_program
let print_env = sexp_println Env.sexp_of_t
let print_expr = sexp_println Ast.sexp_of_expr
let print_typ = sexp_println Type.sexp_of_t
let print_kind = sexp_println Kind.sexp_of_t
let print_kinds = sexp_println (Map.sexp_of_m__t (module Tvar) Kind.sexp_of_t)

let solve program =
  print_endline "program:";
  print_program program;
  print_endline "\n";

  let env, kinds = Solve.solve_exn program in

  print_endline "env:";
  print_env env;
  print_endline "\n";

  print_endline "kinds:";
  print_kinds kinds;
  print_endline "\n"

let () =
  parse_stdin solve
