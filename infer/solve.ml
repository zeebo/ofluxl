open Err

open Ofluxl_std
open Ofluxl_types
open Ofluxl_syntax

(*
 * constraint generation
 *)

let add_kind_constraint ctx typ kind =
  match Type.unwrap typ with
  | Type.Variable name -> Ctx.add_kind_constraint ctx (name, kind)
  | _ -> ()

let rec generate_typ ctx env = function
  (* identifier lookup *)
  | Ast.Ident ident -> begin
      match Env.find env ident with
      | Some scheme -> Ctx.inst ctx scheme
      | None -> raise @@ Error (UnknownIdentifier ident)
    end

  (* basic types *)
  | Integer _ -> Type.wrap @@ Basic Integer
  | Float _ -> Type.wrap @@ Basic Float
  | Duration _ -> Type.wrap @@ Basic Duration
  | Time _ -> Type.wrap @@ Basic Time
  | Regex _ -> Type.wrap @@ Basic Regex
  | Char _ -> Type.wrap @@ Basic Char
  | String _ -> Type.wrap @@ Basic String

  (* math expressions *)
  | Uminus expr ->
    let typ = generate_typ ctx env expr in
    add_kind_constraint ctx typ @@ Kind.wrap @@ Cls Num;
    typ

  | Plus (left, right) ->
    let typl = generate_typ ctx env left in
    let typr = generate_typ ctx env right in
    Ctx.add_typ_constraint ctx (typl, typr);
    add_kind_constraint ctx typl @@ Kind.wrap @@ Cls Add;
    add_kind_constraint ctx typr @@ Kind.wrap @@ Cls Add;
    typl

  | Minus (left, right)
  | Times (left, right)
  | Div (left, right) ->
    let typl = generate_typ ctx env left in
    let typr = generate_typ ctx env right in
    Ctx.add_typ_constraint ctx (typl, typr);
    add_kind_constraint ctx typl @@ Kind.wrap @@ Cls Num;
    add_kind_constraint ctx typr @@ Kind.wrap @@ Cls Num;
    typl

  (* logical operations *)
  | And (left, right)
  | Or (left, right) ->
    let typl = generate_typ ctx env left in
    let typr = generate_typ ctx env right in
    Ctx.add_typ_constraint ctx (typl, Type.wrap @@ Basic Bool);
    Ctx.add_typ_constraint ctx (typr, Type.wrap @@ Basic Bool);
    Type.wrap @@ Basic Bool

  (* function support *)
  | Func (args, body) ->
    let args, table, required = generate_func_args ctx env args in
    let schemes = Map.map args ~f:(fun typ -> (typ, Set.empty (module Tvar))) in
    let env' = Env.insert env schemes in
    let ret = generate_typ ctx env' body in
    Type.wrap @@ Func { args; table; required; ret }

  | Call (expr, args) ->
    let typl = generate_typ ctx env expr in
    let args, required = generate_call_args ctx env args in
    let ret = Type.wrap @@ Variable (Ctx.fresh_type_name ctx) in
    (* TODO: can you call a function with a `in=<-` style argument somehow? *)
    Ctx.add_typ_constraint ctx (typl, Type.wrap @@ Func { args; table = false; required; ret });
    ret

  | Pipe (left, Call (right, args)) ->
    let typl = generate_typ ctx env left in
    let typr = generate_typ ctx env right in
    let args, required = generate_call_args ctx env args in
    let ret = Type.wrap @@ Variable (Ctx.fresh_type_name ctx) in
    Ctx.add_typ_constraint ctx (typl, Type.wrap @@ Basic Table);
    Ctx.add_typ_constraint ctx (typr, Type.wrap @@ Func { args; table = true; required; ret });
    ret

  | Pipe _ as expr ->
    (* if the right side of the pipe isn't a call, it's a problem *)
    raise (Ast.Invalid expr)

  | Return expr -> generate_typ ctx env expr

  (* composite types *)
  | List exprs ->
    let typ = Type.wrap @@ Variable (Ctx.fresh_type_name ctx) in
    let typs = List.map exprs ~f:(generate_typ ctx env) in
    (* for now, assume lists are homogenous (??) *)
    List.iter typs ~f:(fun typ' -> Ctx.add_typ_constraint ctx (typ, typ'));
    Type.wrap @@ List typ

  | Record fields ->
    let name = Ctx.fresh_type_name ctx in
    let fields, upper = generate_call_args ctx env fields in
    Ctx.add_kind_constraint ctx
      (name, Kind.wrap @@ Record
         { fields
         ; lower = Set.empty (module String)
         ; upper = Some upper
         });
    Type.wrap @@ Variable name

  (* projections *)
  | Select (expr, field) ->
    let typf = Type.wrap @@ Variable (Ctx.fresh_type_name ctx) in
    let record = generate_typ ctx env expr in
    begin match Type.unwrap @@ record with
      | Variable name ->
        Ctx.add_kind_constraint ctx
          (name, Kind.wrap @@ Record
             { fields = Map.singleton (module String) field typf
             ; lower = Set.singleton (module String) field
             ; upper = None (* universe *)
             });
      | _ -> raise @@ Error (InvalidType (Fix.typ record))
    end;
    typf

  | Index (expr, index) ->
    let typ = Type.wrap @@ Variable (Ctx.fresh_type_name ctx) in
    let typl = generate_typ ctx env expr in
    let typi = generate_typ ctx env index in
    Ctx.add_typ_constraint ctx (typl, Type.wrap @@ List typ);
    Ctx.add_typ_constraint ctx (typi, Type.wrap @@ Basic Integer);
    typ

  (* comparisons *)
  | Comp (left, _, right) ->
    let typl = generate_typ ctx env left in
    let typr = generate_typ ctx env right in
    Ctx.add_typ_constraint ctx (typl, typr);
    add_kind_constraint ctx typl @@ Kind.wrap @@ Cls Cmp;
    add_kind_constraint ctx typr @@ Kind.wrap @@ Cls Cmp;
    Type.wrap @@ Basic Bool

(* generate a type for a given default argument *)
and generate_default ctx env = function
  | Some (Ast.DExpr expr) -> generate_typ ctx env expr
  | Some DPipe -> Type.wrap @@ Basic Table
  | None -> Type.wrap @@ Variable (Ctx.fresh_type_name ctx)

(* generate a type for a function *)
and generate_func_args ctx env args =
  let table =
    args
    |> List.exists ~f:(fun (_, def) ->
        match def with
        | Some Ast.DPipe -> true
        | _ -> false)
  in

  let required =
    args
    |> List.filter_map ~f:(fun (name, def) ->
        match def with
        | Some _ -> None
        | None -> Some name)
    |> Set.of_list (module String)
  in

  let args =
    args
    |> List.map ~f:(fun (name, def) -> (name, generate_default ctx env def))
    |> Map.of_alist_exn (module String)
  in

  ( args, table, required)

(* generate a type for a call *)
and generate_call_args ctx env args =
  let args =
    args
    |> List.map ~f:(fun (name, expr) -> (name, generate_typ ctx env expr))
    |> Map.of_alist_exn (module String)
  in

  ( args, Set.of_list (module String) @@ Map.keys args )

(*
 * type/kind unification
 *)

let rec unify_typs_exn kinds left right =
  let kinds, subst = match (left, right) with
    | ({ contents = Type.Variable namel }, { contents = Type.Variable namer }) ->
      if Tvar.equal namel namer then kinds, Subst.empty
      else begin
        let kinds, subst = unify_kinds_by_name_exn kinds namel namer in
        kinds, Subst.merge (Subst.singleton namel (Type.wrap @@ Variable namer)) subst
      end

    | ({ contents = Variable name }, typ)
    | (typ, { contents = Variable name }) ->
      if Type.occurs name typ then raise @@ Error (Infinite (name, Fix.typ typ))
      else
        let kinds = unify_kinds_by_typ_exn kinds name typ in
        kinds, Subst.singleton name typ

    | ({ contents = List left }, {contents = List right }) -> unify_typs_exn kinds left right

    | ({ contents = Func {args = argsl; table = tablel; required = requiredl; ret = retl }},
       { contents = Func {args = argsr; table = tabler; required = requiredr; ret = retr }}) ->

      (* TODO: allow calls with the table being explicitly specified *)
      if not @@ Bool.equal tablel tabler ||
         not @@ Set.is_subset requiredl ~of_:requiredr
      then
        raise @@ Error (MismatchedTypes (Fix.typ left, Fix.typ right))
      else begin
        Map.fold2 argsl argsr
          ~init:(unify_typs_exn kinds retl retr)
          ~f:(fun ~key:name ~data:data (kinds, subst) ->
              match data with
              | `Left _ ->
                if Set.mem requiredl name
                then raise @@ Error (MismatchedTypes (Fix.typ left, Fix.typ right))
                else kinds, subst
              | `Right _ ->
                raise @@ Error (MismatchedTypes (Fix.typ left, Fix.typ right))
              | `Both (typl, typr) ->
                let typl = Subst.apply_typ subst typl in
                let typr = Subst.apply_typ subst typr in
                let kinds, subst' = unify_typs_exn kinds typl typr in
                kinds, Subst.merge subst' subst
            )
      end

    | ({ contents = Invalid }, { contents = Invalid })
    | ({ contents = Basic Integer }, { contents = Basic Integer })
    | ({ contents = Basic Float }, { contents = Basic Float })
    | ({ contents = Basic Duration }, { contents = Basic Duration })
    | ({ contents = Basic Time }, { contents = Basic Time })
    | ({ contents = Basic Regex }, { contents = Basic Regex })
    | ({ contents = Basic Char }, { contents = Basic Char })
    | ({ contents = Basic String }, { contents = Basic String })
    | ({ contents = Basic Bool }, { contents = Basic Bool })
    | ({ contents = Basic Table }, { contents = Basic Table }) -> kinds, Subst.empty

    | ({ contents = Func _ }, _)
    | ({ contents = List _ }, _)
    | ({ contents = Basic _ }, _)
    | ({ contents = Invalid }, _) ->
      raise @@ Error (MismatchedTypes (Fix.typ left, Fix.typ right))
  in

  (Map.map kinds ~f:(Subst.apply_kind subst), subst)

and unify_kinds_by_typ_exn kinds name typ =
  match Map.find kinds name with
  | None -> kinds
  | Some kind ->
    match (Kind.unwrap kind, Type.unwrap typ) with
    | (Record _, Variable _) -> kinds
    | (Cls Cmp, Basic Integer)  | (Cls Add, Basic Integer)  | (Cls Num, Basic Integer)
    | (Cls Cmp, Basic Float)    | (Cls Add, Basic Float)    | (Cls Num, Basic Float)
    | (Cls Cmp, Basic Duration) | (Cls Add, Basic Duration) | (Cls Num, Basic Duration)
    | (Cls Cmp, Basic String)   | (Cls Add, Basic String)
    | (Cls Cmp, Basic Time)
    | (Cls Cmp, Basic Regex)
    | (Cls Cmp, Basic Char)
    | (Cls Cmp, Basic Bool) -> Map.remove kinds name
    | _ -> raise @@ Error (InvalidTypeForKind (Fix.typ typ, Fix.kind kind))

and unify_kinds kinds namel namer left right =
  let set (data: Kind.t) = Map.set kinds ~key:namer ~data in

  let kinds, subst =
    match (Kind.unwrap left, Kind.unwrap right) with
    | (Kind.Cls Cmp, Kind.Cls Cmp) -> set @@ Kind.wrap @@ Kind.Cls Cmp, Subst.empty
    | (Cls Cmp, Cls Add) -> set @@ Kind.wrap @@ Cls Add, Subst.empty
    | (Cls Cmp, Cls Num) -> set @@ Kind.wrap @@ Cls Num, Subst.empty

    | (Cls Add, Cls Cmp) -> set @@ Kind.wrap @@ Cls Add, Subst.empty
    | (Cls Add, Cls Add) -> set @@ Kind.wrap @@ Cls Add, Subst.empty
    | (Cls Add, Cls Num) -> set @@ Kind.wrap @@ Cls Num, Subst.empty

    | (Cls Num, Cls Cmp) -> set @@ Kind.wrap @@ Cls Num, Subst.empty
    | (Cls Num, Cls Add) -> set @@ Kind.wrap @@ Cls Num, Subst.empty
    | (Cls Num, Cls Num) -> set @@ Kind.wrap @@ Cls Num, Subst.empty

    | (Record { fields = fieldsl; upper = upperl; lower = lowerl },
       Record { fields = fieldsr; upper = upperr; lower = lowerr }) ->

      let kinds, subst = ref kinds, ref Subst.empty in
      let fields = Map.merge fieldsl fieldsr ~f:(fun ~key:_ -> function
          | `Left left -> Some left
          | `Right right -> Some right
          | `Both (left, right) ->
            try
              let kinds', subst' = unify_typs_exn !kinds left right in
              kinds := kinds';
              subst := Subst.merge subst' !subst;
              Some (Subst.apply_typ subst' left)
            with
            | Error _ -> Some (Type.wrap Invalid)
        )
      in
      let upper = match (upperl, upperr) with
        | (None, Some upperr) -> Some upperr
        | (Some upperl, None) -> Some upperl
        | (None, None) -> None
        | (Some upperl, Some upperr) -> Some (Set.inter upperl upperr)
      in
      let lower = Set.union lowerl lowerr in

      begin match upper with
        | Some upper ->
          let extra = Set.diff lower upper in
          if not (Set.is_empty extra)
          then raise @@ Error (UnknownRecordAccess extra)
          else ()
        | None -> ()
      end;

      let kind = Kind.wrap @@ Record { fields; upper; lower } in
      begin if Kind.invalid kind
        then raise @@ Error (InvalidKind (Fix.kind kind))
        else ()
      end;

      let kinds = Map.set !kinds ~key:namer ~data:kind in
      kinds, !subst

    | _ -> raise @@ Error (MismatchedKinds (Fix.kind left, Fix.kind right))
  in

  (* remove the left name if it exists and is distinct *)
  if not (Tvar.equal namel namer) then
    Map.remove kinds namel, subst
  else
    kinds, subst

and unify_kinds_by_name_exn kinds namel namer =
  let kinds, subst =
    match (Map.find kinds namel, Map.find kinds namer) with
    | (Some left, Some right) ->
      unify_kinds kinds namel namer left right

    | (Some kind, None) ->
      let kinds = Map.set kinds ~key:namer ~data:kind in
      let kinds = Map.remove kinds namel in
      kinds, Subst.empty

    | (None, Some _) ->
      kinds, Subst.empty

    | (None, None) ->
      kinds, Subst.empty
  in
  (Map.map kinds ~f:(Subst.apply_kind subst), subst)

(*
 * type solving
 *)

let solve_exn program =
  (* gather constraints *)
  let ctx = Ctx.create () in
  let env = List.fold program ~init:Env.empty ~f:(fun env -> function
      | Ast.Expr expr ->
        let typ = generate_typ ctx env expr in
        let name = "!expr_" ^ (Tvar.to_string (Ctx.fresh_type_name ctx)) in
        Env.set env name (typ, Set.empty (module Tvar))
      | Ast.Assign (ident, expr) ->
        let typ = generate_typ ctx env expr in
        Env.set env ident (typ, Ctx.ftv ctx typ)
    )
  in

  let typ_constraints = Ctx.typ_constraints ctx in
  let kind_constraints = Ctx.kind_constraints ctx in

  (* fold up the substitutions from unification of the constraints *)
  let kind_constraints =
    kind_constraints
    |> Map.to_alist
    |> List.concat_map ~f:(fun (name, kinds) -> List.map kinds ~f:(fun kind -> (name, kind)))
  in
  let kinds = Map.of_alist_reduce (module Tvar) kind_constraints ~f:(fun a _ -> a) in

  let kinds, subst = List.fold kind_constraints
      ~init:(kinds, Subst.empty)
      ~f:(fun (kinds, subst) (name, kind) ->
          let name' = Subst.apply_name subst name in
          match Map.find kinds name' with
          | None -> kinds, subst
          | Some kind' ->
            let kinds', subst' = unify_kinds kinds name name' kind kind' in
            (kinds', Subst.merge subst' subst)
        )
  in

  let kinds, subst =
    List.fold typ_constraints
      ~init:(kinds, subst)
      ~f:(fun (kinds, subst) (left, right) ->
          let left = Subst.apply_typ subst left in
          let right = Subst.apply_typ subst right in
          let kinds', subst' = unify_typs_exn kinds left right in
          kinds', Subst.merge subst' subst)
  in

  (* apply the substitutions *)
  let kinds = Map.map kinds ~f:(Subst.apply_kind subst) in
  let env = Subst.apply_env subst env in

  let kinds = Map.map kinds ~f:Fix.kind in
  let env = Fix.env env in

  (env, kinds)

let solve expr =
  try Ok (solve_exn expr) with
  | Error error -> Error error
