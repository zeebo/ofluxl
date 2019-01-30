open Err

open Ofluxl_std
open Ofluxl_types
open Ofluxl_syntax

let solve_exn program =
  let names = object
    val mutable n = 0
    method fresh =
      n <- n + 1;
      sprintf ".expr%d" n
  end in

  let ctx = Context.default () in

  (* first generate all of the initial constraints from the syntax *)
  List.iter program ~f:(function
      | Ast.Expr expr ->
        let typ = Generate.generate ctx expr in
        ctx#insert names#fresh (Scheme.empty typ)
      | Ast.Assign (ident, expr) ->
        let typ = Generate.generate ctx expr in
        ctx#insert ident (ctx#generalize typ));

  (* helper recursively builds up a substitution and set of kinds from constraints *)
  let rec helper subst kinds constraints =
    (* create a new context to hold constraints generated during this run *)
    let ctx = Context.default () in

    (* a helper to add a type constraint and copy any kind constraints *)
    let update_subst subst typo typn =
      match Type.substitute subst typo with
      | Type.Variable name ->
        begin match Map.find kinds name with
          | Some kind -> ctx#kind_constraint typn kind
          | None -> ()
        end;
        Subst.insert subst name typn
      | _ -> subst
    in

    (* first, load all of the kinds in *)
    let kinds = List.fold constraints ~init:kinds ~f:(fun kinds -> function
        | Context.Kind (name, kind) -> begin
            let kind = Kind.substitute subst kind in
            let kind = match Map.find kinds name with
              | None -> kind
              | Some kind' -> Unify.kinds ctx kind kind'
            in
            let kind = Kind.substitute subst kind in
            Map.set kinds ~key:name ~data:kind
          end
        | _ -> kinds)
    in

    (* next, run all the inst constraints *)
    List.iter constraints ~f:(function
        | Context.Inst subst' -> Subst.iteri subst' ~f:(fun ~key:name ~data:typ ->
            match Map.find kinds name with
            | Some kind -> ctx#kind_constraint (Type.substitute subst typ) (Kind.substitute subst' kind)
            | None -> ())
        | _ -> ());

    (* finally, run all the type constraints *)
    let subst = List.fold constraints ~init:subst ~f:(fun subst -> function
        | Context.Type (typl, typr) ->
          let typl = Type.substitute subst typl in
          let typr = Type.substitute subst typr in
          let typ = Unify.typs ctx typl typr in
          let subst = update_subst subst typl typ in
          let subst = update_subst subst typr typ in
          subst
        | _ -> subst)
    in

    (* update all the kinds with any substitutions and fold them *)
    let kinds_list = Map.to_alist kinds in
    let kinds_list = List.filter_map kinds_list ~f:(fun (name, kind) ->
        let kind = Kind.substitute subst kind in
        match Type.substitute subst (Variable name) with
        | Variable name -> Some (name, kind)
        | typ ->
          if not @@ Kind.compatible_with_typ kind typ
          then throw @@ InvalidTypeForKind (typ, kind)
          else None)
    in
    let kinds = Map.of_alist_reduce (module Tvar) kinds_list ~f:(Unify.kinds ctx) in

    (* recurse if we added any new constraints *)
    match ctx#constraints with
    | [] -> subst, kinds
    | constraints -> helper subst kinds constraints
  in

  let subst, kinds = helper Subst.empty (Map.empty (module Tvar)) ctx#constraints in
  Env.substitute subst ctx#env, kinds
