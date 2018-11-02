open Ofluxl
open Std

module Foo = Infer.Context

let print_program =
  sexp_println Syntax.Ast.sexp_of_program

let print_env =
  (* sexp_println Infer.Env.sexp_of_t *)
  sexp_println (Hashtbl.sexp_of_m__t (module String) Ofluxl.Infer.Scheme.sexp_of_t)

let print_kinds =
  sexp_println (Hashtbl.sexp_of_m__t (module Ofluxl.Types.Tvar) Ofluxl.Types.Kind.sexp_of_t)

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
