open Std

let parse cont lexbuf =
  try cont (Parser.main Lexer.token lexbuf) with
  | Lexer.Error _ -> Err.print_error @@ Err.Lexing lexbuf
  | Parser.Error -> Err.print_error @@ Err.Parsing lexbuf

let parse_stdin cont = Lexing.from_channel Stdio.stdin |> parse cont
let parse_str cont str = Lexing.from_string str |> parse cont

let print_program = sprint Ast.sexp_of_program
let print_env = sprint Env.sexp_of_t
let print_expr = sprint Ast.sexp_of_expr
let print_typ = sprint Types.sexp_of_typ
let print_kind = sprint Types.sexp_of_kind
let print_kinds = sprint (Map.sexp_of_m__t (module String) Types.sexp_of_kind)

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
