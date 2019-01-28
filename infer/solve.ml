open Ofluxl_std
open Ofluxl_types
open Ofluxl_syntax

let solver (ctx: Context.t) = (object (self)
  val mutable subst = Subst.empty
  val mutable kinds = Map.empty (module Tvar)

  method update_subst typo typn =
    match typo with
    | Type.Variable name ->
      subst <- Subst.insert subst name typn;
      begin match Map.find kinds name with
        | None -> ()
        | Some kind -> ctx#kind_constraint typn kind
      end
    | _ -> ()

  method apply = function
    | Context.Type (typl, typr) ->
      let typ = Unify.typs ctx typl typr in
      self#update_subst typl typ;
      self#update_subst typr typ

    | Kind (name, kind) -> begin
        let kind = 
          match Map.find kinds name with
          | None -> kind
          | Some kind' -> Unify.kinds ctx kind kind'
        in
        kinds <- Map.set kinds ~key:name ~data:kind
      end

    | Inst subst ->
      Subst.iteri subst ~f:(fun ~key:name ~data:typ ->
          match Map.find kinds name with
          | None -> ()
          | Some kind ->
            let kind = Kind.substitute subst kind in
            ctx#kind_constraint typ kind
        )

  method solve =
    let rec helper = function
      | None -> 
        subst, kinds
      | Some constr ->
        sexp_println Context.sexp_of_constr constr;
        self#apply constr;
        helper ctx#pop_constraint
    in

    helper ctx#pop_constraint
end)

let solve_exn program =
  let names = object
    val mutable n = 0
    method fresh =
      n <- n + 1;
      sprintf ".expr%d" n
  end in

  let ctx = Context.default () in

  List.iter program ~f:(function
      | Ast.Expr expr ->
        let typ = Generate.generate ctx expr in
        ctx#insert names#fresh (Scheme.empty typ)
      | Ast.Assign (ident, expr) ->
        let typ = Generate.generate ctx expr in
        ctx#insert ident (ctx#generalize typ));

  let subst, kinds = (solver ctx)#solve in
  Env.substitute subst ctx#env, Map.map kinds ~f:(Kind.substitute subst)
