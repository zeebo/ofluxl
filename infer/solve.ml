open Ofluxl_std
open Ofluxl_syntax

let solve_exn program =
  let names = object
    val mutable n = 0
    method fresh =
      n <- n + 1;
      sprintf "!expr%d" n
  end in

  let ctx = Context.default () in

  List.iter program ~f:(function
      | Ast.Expr expr ->
        let typ = Generate.generate ctx expr in
        ctx#insert names#fresh (ctx#generalize typ)
      | Ast.Assign (ident, expr) ->
        let typ = Generate.generate ctx expr in
        ctx#insert ident (ctx#generalize typ));

  ctx#solve Unify.unify
