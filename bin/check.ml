open Ofluxl
open Std

let print_program =
  sexp_println Syntax.Ast.sexp_of_program

let print_env =
  sexp_println Infer.Env.Fixed.sexp_of_t

let print_kinds =
  sexp_println (Map.sexp_of_m__t (module Types.Tvar) Types.Kind.Fixed.sexp_of_t)

let solve program =
  println "program:";
  print_program program;
  println "";

  let env, kinds = Infer.Solve.solve_exn program in

  println "env:";
  print_env env;
  println "";

  println "kinds:";
  print_kinds kinds;
  println ""

let () =
  match Syntax.Parse.stdin () with
  | Ok program -> solve program
  | Error err -> Syntax.Err.print err
