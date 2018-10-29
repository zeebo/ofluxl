open Std

type error =
  | Infinite of (Tvar.t * Type.t)
  | MismatchedKinds of (Kind.t * Kind.t)
  | MismatchedTypes of (Type.t * Type.t)
  | InvalidSubst of (string * Type.t)
  | InvalidTypeForKind of (Type.t * Kind.t)
  | InvalidKind of Kind.t
  | InvalidType of Type.t
  | UnknownIdentifier of string
  | UnknownRecordAccess of Set.M(String).t
[@@deriving sexp_of]

exception Error of error
[@@deriving sexp_of]

(*
 * constraint generation
 *)

let add_kind_constraint ctx typ kind =
  match typ with
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
  | Integer _ -> Basic Integer
  | Float _ -> Basic Float
  | Duration _ -> Basic Duration
  | Time _ -> Basic Time
  | Regex _ -> Basic Regex
  | Char _ -> Basic Char
  | String _ -> Basic String

  (* math expressions *)
  | Uminus expr ->
    let typ = generate_typ ctx env expr in
    add_kind_constraint ctx typ @@ Cls Num;
    typ

  | Plus (left, right) ->
    let typl = generate_typ ctx env left in
    let typr = generate_typ ctx env right in
    Ctx.add_typ_constraint ctx (typl, typr);
    add_kind_constraint ctx typl @@ Cls Add;
    add_kind_constraint ctx typr @@ Cls Add;
    typl

  | Minus (left, right)
  | Times (left, right)
  | Div (left, right) ->
    let typl = generate_typ ctx env left in
    let typr = generate_typ ctx env right in
    Ctx.add_typ_constraint ctx (typl, typr);
    add_kind_constraint ctx typl @@ Cls Num;
    add_kind_constraint ctx typr @@ Cls Num;
    typl

  (* logical operations *)
  | And (left, right)
  | Or (left, right) ->
    let typl = generate_typ ctx env left in
    let typr = generate_typ ctx env right in
    Ctx.add_typ_constraint ctx (typl, Basic Bool);
    Ctx.add_typ_constraint ctx (typr, Basic Bool);
    Basic Bool

  (* function support *)
  | Func (args, body) ->
    let args, table, required = generate_func_args ctx env args in
    let schemes = Map.map args ~f:(fun typ -> (typ, Set.empty (module Tvar))) in
    let env' = Env.insert env schemes in
    let ret = generate_typ ctx env' body in
    Func { args; table; required; ret }

  | Call (expr, args) ->
    let typl = generate_typ ctx env expr in
    let args, required = generate_call_args ctx env args in
    let ret = Type.Variable (Ctx.fresh_type_name ctx) in
    (* TODO: can you call a function with a `in=<-` style argument somehow? *)
    Ctx.add_typ_constraint ctx (typl, Func { args; table = false; required; ret });
    ret

  | Pipe (left, Call (right, args)) ->
    let typl = generate_typ ctx env left in
    let typr = generate_typ ctx env right in
    let args, required = generate_call_args ctx env args in
    let ret = Type.Variable (Ctx.fresh_type_name ctx) in
    Ctx.add_typ_constraint ctx (typl, Basic Table);
    Ctx.add_typ_constraint ctx (typr, Func { args; table = true; required; ret });
    ret

  | Pipe _ as expr ->
    (* if the right side of the pipe isn't a call, it's a problem *)
    raise (Ast.Invalid expr)

  | Return expr -> generate_typ ctx env expr

  (* composite types *)
  | List exprs ->
    let typ = Type.Variable (Ctx.fresh_type_name ctx) in
    let typs = List.map exprs ~f:(generate_typ ctx env) in
    (* for now, assume lists are homogenous (??) *)
    List.iter typs ~f:(fun typ' -> Ctx.add_typ_constraint ctx (typ, typ'));
    List typ

  | Record fields ->
    let name = Ctx.fresh_type_name ctx in
    let fields, upper = generate_call_args ctx env fields in
    Ctx.add_kind_constraint ctx
      (name, Record { fields
                    ; lower = Set.empty (module String)
                    ; upper = Some upper
                    });
    Variable name

  (* projections *)
  | Select (expr, field) ->
    let typf = Type.Variable (Ctx.fresh_type_name ctx) in
    begin match generate_typ ctx env expr with
      | Variable name ->
        Ctx.add_kind_constraint ctx
          (name, Record { fields = Map.singleton (module String) field typf
                        ; lower = Set.singleton (module String) field
                        ; upper = None (* universe *)
                        });
      | _ as typ -> raise @@ Error (InvalidType typ)
    end ;
    typf

  | Index (expr, index) ->
    let typ = Type.Variable (Ctx.fresh_type_name ctx) in
    let typl = generate_typ ctx env expr in
    let typi = generate_typ ctx env index in
    Ctx.add_typ_constraint ctx (typl, List typ);
    Ctx.add_typ_constraint ctx (typi, Basic Integer);
    typ

  (* comparisons *)
  | Comp (left, _, right) ->
    let typl = generate_typ ctx env left in
    let typr = generate_typ ctx env right in
    Ctx.add_typ_constraint ctx (typl, typr);
    add_kind_constraint ctx typl @@ Cls Cmp;
    add_kind_constraint ctx typr @@ Cls Cmp;
    Basic Bool

(* generate a type for a given default argument *)
and generate_default ctx env = function
  | Some (Ast.DExpr expr) -> generate_typ ctx env expr
  | Some DPipe -> Basic Table
  | None -> Variable (Ctx.fresh_type_name ctx)

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
    | (Type.Variable namel, Type.Variable namer) ->
      if Tvar.equal namel namer then kinds, Subst.empty
      else begin
        let kinds, subst = unify_kinds_by_name_exn kinds namel namer in
        kinds, Subst.merge (Subst.singleton namel (Variable namer)) subst
      end

    | (Variable name, typ)
    | (typ, Variable name) ->
      if Type.occurs name typ then raise @@ Error (Infinite (name, typ))
      else
        let kinds = unify_kinds_by_typ_exn kinds name typ in
        kinds, Subst.singleton name typ

    | (List left, List right) -> unify_typs_exn kinds left right

    | (Func {args = argsl; table = tablel; required = requiredl; ret = retl },
       Func {args = argsr; table = tabler; required = requiredr; ret = retr }) as typs ->

      (* TODO: allow calls with the table being explicitly specified *)
      if not @@ Bool.equal tablel tabler ||
         not @@ Set.is_subset requiredl ~of_:requiredr
      then
        raise @@ Error (MismatchedTypes typs)
      else begin
        Map.fold2 argsl argsr
          ~init:(unify_typs_exn kinds retl retr)
          ~f:(fun ~key:name ~data:data (kinds, subst) ->
              match data with
              | `Left _ ->
                if Set.mem requiredl name
                then raise @@ Error (MismatchedTypes typs)
                else kinds, subst
              | `Right _ ->
                raise @@ Error (MismatchedTypes typs)
              | `Both (typl, typr) ->
                let typl = Subst.apply_typ subst typl in
                let typr = Subst.apply_typ subst typr in
                let kinds, subst' = unify_typs_exn kinds typl typr in
                kinds, Subst.merge subst' subst
            )
      end

    | (Invalid, Invalid)
    | (Basic Integer, Basic Integer)
    | (Basic Float, Basic Float)
    | (Basic Duration, Basic Duration)
    | (Basic Time, Basic Time)
    | (Basic Regex, Basic Regex)
    | (Basic Char, Basic Char)
    | (Basic String, Basic String)
    | (Basic Bool, Basic Bool)
    | (Basic Table, Basic Table) -> kinds, Subst.empty

    | (Func _, _) | (List _, _) | (Basic _, _) | (Invalid, _) as typs ->
      raise @@ Error (MismatchedTypes typs)
  in

  (Map.map kinds ~f:(Subst.apply_kind subst), subst)

and unify_kinds_by_typ_exn kinds name typ =
  match Map.find kinds name with
  | None -> kinds

  | Some (Cls cls as kind) -> begin
      match typ with
      | Basic basic ->
        begin match (basic, cls) with
          | (Integer,  Cmp) | (Integer,  Add) | (Integer,  Num)
          | (Float,    Cmp) | (Float,    Add) | (Float,    Num)
          | (Duration, Cmp) | (Duration, Add) | (Duration, Num)
          | (String,   Cmp) | (String,   Add)
          | (Time,     Cmp)
          | (Regex,    Cmp)
          | (Char,     Cmp)
          | (Bool,     Cmp) -> Map.remove kinds name
          | _ -> raise @@ Error (InvalidTypeForKind (typ, kind))
        end
      | _ -> raise @@ Error (InvalidTypeForKind (typ, kind))
    end

  | Some (Record _ as kind) -> begin
      match typ with
      | Variable _ -> kinds
      | _ -> raise @@ Error (InvalidTypeForKind (typ, kind))
    end

and unify_kinds kinds namel namer left right =
  let set data = Map.set kinds ~key:namer ~data in

  let kinds, subst =
    match (left, right) with
    | (Kind.Cls Cmp, Kind.Cls Cmp) -> set @@ Kind.Cls Cmp, Subst.empty
    | (Cls Cmp, Cls Add) -> set @@ Cls Add, Subst.empty
    | (Cls Cmp, Cls Num) -> set @@ Cls Num, Subst.empty

    | (Cls Add, Cls Cmp) -> set @@ Cls Add, Subst.empty
    | (Cls Add, Cls Add) -> set @@ Cls Add, Subst.empty
    | (Cls Add, Cls Num) -> set @@ Cls Num, Subst.empty

    | (Cls Num, Cls Cmp) -> set @@ Cls Num, Subst.empty
    | (Cls Num, Cls Add) -> set @@ Cls Num, Subst.empty
    | (Cls Num, Cls Num) -> set @@ Cls Num, Subst.empty

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
            | Error _ -> Some Invalid
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

      let kind = Kind.Record { fields; upper; lower } in
      begin if Kind.invalid kind
        then raise @@ Error (InvalidKind kind)
        else ()
      end;

      let kinds = Map.set !kinds ~key:namer ~data:kind in
      kinds, !subst

    | _ -> raise @@ Error (MismatchedKinds (left, right))
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

  (env, kinds)

let solve expr =
  try Ok (solve_exn expr) with
  | Error error -> Error error
