open Std
open Types

type error =
  | Infinite of (tvar * typ)
  | MismatchedKinds of (kind * kind)
  | MismatchedTypes of (typ * typ)
  | InvalidSubst of (string * typ)
  | InvalidTypeForKind of (typ * kind)
  | InvalidKind of kind
  | UnknownIdentifier of string

exception Error of error

(*
 * constraint generation
 *)

let rec generate_typ ctx env = function
  (* identifier lookup *)
  | Ast.Ident ident -> begin
      match Env.find env ident with
      | Some typ -> typ
      | None -> raise @@ Error (UnknownIdentifier ident)
    end

  (* basic types *)
  | Ast.Integer _ -> Basic Integer
  | Ast.Float _ -> Basic Float
  | Ast.Duration _ -> Basic Duration
  | Ast.Time _ -> Basic Time
  | Ast.Regex _ -> Basic Regex
  | Ast.Char _ -> Basic Char
  | Ast.String _ -> Basic String

  (* math expressions *)
  | Ast.Uminus expr ->
    let name = Ctx.fresh_type_name ctx in
    let typ = generate_typ ctx env expr in
    Ctx.add_kind_constraint ctx (name, KCls Num);
    Ctx.add_typ_constraint ctx (Variable name, typ);
    typ

  | Ast.Plus (left, right) ->
    let namel = Ctx.fresh_type_name ctx in
    let typl = generate_typ ctx env left in
    let namer = Ctx.fresh_type_name ctx in
    let typr = generate_typ ctx env right in
    Ctx.add_typ_constraint ctx (typl, typr);
    Ctx.add_kind_constraint ctx (namel, KCls Add);
    Ctx.add_typ_constraint ctx (Variable namel, typl);
    Ctx.add_kind_constraint ctx (namer, KCls Add);
    Ctx.add_typ_constraint ctx (Variable namer, typr);
    Ctx.add_typ_constraint ctx (Variable namel, Variable namer);
    typl

  | Ast.Minus (left, right)
  | Ast.Times (left, right)
  | Ast.Div (left, right) ->
    let namel = Ctx.fresh_type_name ctx in
    let typl = generate_typ ctx env left in
    let namer = Ctx.fresh_type_name ctx in
    let typr = generate_typ ctx env right in
    Ctx.add_typ_constraint ctx (typl, typr);
    Ctx.add_kind_constraint ctx (namel, KCls Num);
    Ctx.add_typ_constraint ctx (Variable namel, typl);
    Ctx.add_kind_constraint ctx (namer, KCls Num);
    Ctx.add_typ_constraint ctx (Variable namer, typr);
    Ctx.add_typ_constraint ctx (Variable namel, Variable namer);
    typl

  (* logical operations *)
  | Ast.And (left, right)
  | Ast.Or (left, right) ->
    let typl = generate_typ ctx env left in
    let typr = generate_typ ctx env right in
    Ctx.add_typ_constraint ctx (typl, Basic Bool);
    Ctx.add_typ_constraint ctx (typr, Basic Bool);
    Basic Bool

  (* function support *)
  | Ast.Func (args, body) ->
    let args, table, required = generate_func_args ctx env args in
    let env' = Env.insert env args in
    let ret = generate_typ ctx env' body in
    Func { args; table; required; ret }

  | Ast.Call (expr, args) ->
    let typl = generate_typ ctx env expr in
    let args, required = generate_call_args ctx env args in
    let ret = Variable (Ctx.fresh_type_name ctx) in
    (* TODO: can you call a function with a `in=<-` style argument somehow? *)
    Ctx.add_typ_constraint ctx (typl, Func { args; table = false; required; ret });
    ret

  | Ast.Pipe (left, Ast.Call (right, args)) ->
    let typl = generate_typ ctx env left in
    let typr = generate_typ ctx env right in
    let args, required = generate_call_args ctx env args in
    let ret = Variable (Ctx.fresh_type_name ctx) in
    Ctx.add_typ_constraint ctx (typl, Basic Table);
    Ctx.add_typ_constraint ctx (typr, Func { args; table = true; required; ret });
    ret

  | Ast.Pipe _ as expr ->
    (* if the right side of the pipe isn't a call, it's a problem *)
    raise (Ast.Invalid expr)

  | Ast.Return expr -> generate_typ ctx env expr

  (* composite types *)
  | Ast.List exprs ->
    let typ = Variable (Ctx.fresh_type_name ctx) in
    let typs = List.map exprs ~f:(generate_typ ctx env) in
    (* for now, assume lists are homogenous (??) *)
    List.iter typs ~f:(fun typ' -> Ctx.add_typ_constraint ctx (typ, typ'));
    List typ

  | Ast.Record fields ->
    let name = Ctx.fresh_type_name ctx in
    let fields, upper = generate_call_args ctx env fields in
    Ctx.add_kind_constraint ctx
      (name, KRecord { fields
                     ; lower = Set.empty (module String)
                     ; upper = Some upper
                     });
    Record name

  (* projections *)
  | Ast.Select (expr, field) ->
    let name = Ctx.fresh_type_name ctx in
    let typf = Variable (Ctx.fresh_type_name ctx) in
    let typ = generate_typ ctx env expr in
    Ctx.add_typ_constraint ctx (typ, Record name);
    Ctx.add_kind_constraint ctx
      (name, KRecord { fields = Map.singleton (module String) field typf
                     ; lower = Set.singleton (module String) field
                     ; upper = None (* universe *)
                     });
    typf

  | Ast.Index (expr, index) ->
    let typ = Variable (Ctx.fresh_type_name ctx) in
    let typl = generate_typ ctx env expr in
    let typi = generate_typ ctx env index in
    Ctx.add_typ_constraint ctx (typl, List typ);
    Ctx.add_typ_constraint ctx (typi, Basic Integer);
    typ

  (* assignment *)
  | Ast.Assign (_name, expr) ->
    let typ = generate_typ ctx env expr in
    (* TODO: we need to have an idea of what statements come
     * after in order for assignment to make any sense. for
     * now assume assignment returns the value that was
     * assigned.
    *)
    typ

  (* comparisons *)
  | Ast.Comp (left, _, right) ->
    let namel = Ctx.fresh_type_name ctx in
    let typl = generate_typ ctx env left in
    let namer = Ctx.fresh_type_name ctx in
    let typr = generate_typ ctx env right in
    Ctx.add_typ_constraint ctx (Variable namel, typl);
    Ctx.add_kind_constraint ctx (namel, KCls Cmp);
    Ctx.add_typ_constraint ctx (Variable namer, typr);
    Ctx.add_kind_constraint ctx (namer, KCls Cmp);
    Ctx.add_typ_constraint ctx (typl, typr);
    Basic Bool

(* generate a type for a given default argument *)
and generate_default ctx env = function
  | Some (Ast.DExpr expr) -> generate_typ ctx env expr
  | Some Ast.DPipe -> Basic Table
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

let rec unify_typs_exn ?(depth=0) kinds left right =
  let indent = String.init depth ~f:(fun _ -> '\t') in

  printf "%sunify: " indent;
  Sexp.List [sexp_of_typ left; sexp_of_typ right] |> Sexp.to_string_hum |> print_endline;

  let kinds, subst = match (left, right) with
    | (Record namel, Record namer)
    | (Variable namel, Record namer)
    | (Record namer, Variable namel) ->
      if String.equal namel namer then kinds, Subst.empty
      else
        let kinds, subst = unify_kinds_by_name_exn ~depth:(depth+1) kinds namel namer in
        kinds, Subst.merge subst (Subst.singleton namel (Record namer))

    | (Variable namel, Variable namer) ->
      if String.equal namel namer then kinds, Subst.empty
      else
        let kinds, subst = unify_kinds_by_name_exn ~depth:(depth+1) kinds namel namer in
        kinds, Subst.merge subst (Subst.singleton namel (Variable namer))

    | (Variable name, typ) | (typ, Variable name) ->
      if Types.occurs name typ then raise @@ Error (Infinite (name, typ))
      else
        let kinds = unify_kinds_by_typ_exn kinds name typ in
        kinds, Subst.singleton name typ

    | (List left, List right) -> unify_typs_exn ~depth:(depth+1) kinds left right

    | (Func {args = argsl; table = tablel; required = requiredl; ret = retl },
       Func {args = argsr; table = tabler; required = requiredr; ret = retr}) as typs ->

      let namesl = List.sort ~compare:String.compare @@ Map.keys argsl in
      let namesr = List.sort ~compare:String.compare @@ Map.keys argsr in

      if not @@ Bool.equal tablel tabler ||
         not @@ List.equal namesl namesr ~equal:String.equal ||
         not @@ Set.is_subset requiredl ~of_:requiredr
      then
        raise @@ Error (MismatchedTypes typs)
      else begin
        List.fold namesl
          ~init:(unify_typs_exn ~depth:(depth+1) kinds retl retr)
          ~f:(fun (kinds, subst) name ->
              let typl = Map.find_exn argsl name in
              let typr = Map.find_exn argsr name in
              let kinds, subst' = unify_typs_exn ~depth:(depth+1) kinds typl typr in
              kinds, Subst.merge subst' subst)
      end

    | (left, right) as typs ->
      if phys_equal left right then kinds, Subst.empty
      else raise @@ Error (MismatchedTypes typs)
  in

  (Map.map kinds ~f:(Subst.apply_kind subst), subst)

and unify_kinds_by_typ_exn kinds name typ =
  match Map.find kinds name with
  | None -> kinds

  | Some (KCls _ as kind) -> begin
      match typ with
      | Basic _ -> Map.remove kinds name
      (* TODO: more checking based on the cls *)
      | _ -> raise @@ Error (InvalidTypeForKind (typ, kind))
    end

  | Some (KRecord _ as kind) -> begin
      match typ with
      | Variable _ -> kinds
      | _ -> raise @@ Error (InvalidTypeForKind (typ, kind))
    end

and unify_kinds_by_name_exn ?(depth=0) kinds namel namer =
  let indent = String.init depth ~f:(fun _ -> '\t') in

  printf "%sunify kinds: %s => %s\n" indent namel namer;

  let kinds, subst =
    match (Map.find kinds namel, Map.find kinds namer) with
    | (Some KCls _, Some KCls _) ->
      let kinds = Map.remove kinds namel in
      kinds, Subst.empty

    | (Some KRecord { fields = fieldsl; upper = upperl; lower = lowerl },
       Some KRecord { fields = fieldsr; upper = upperr; lower = lowerr }) ->

      let kinds, subst = ref kinds, ref Subst.empty in
      let fields = Map.merge fieldsl fieldsr ~f:(fun ~key:_ -> function
          | `Left left -> Some left
          | `Right right -> Some right
          | `Both (left, right) ->
            try
              let kinds', subst' = unify_typs_exn ~depth:(depth+1) !kinds left right in
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

      let kinds = Map.remove !kinds namel in
      let kinds = Map.set kinds ~key:namer ~data:(KRecord { fields; upper; lower }) in
      kinds, !subst

    | (Some kind, None) ->
      let kinds = Map.remove kinds namel in
      let kinds = Map.set kinds ~key:namer ~data:kind in
      kinds, Subst.empty

    | (None, Some _) ->
      kinds, Subst.empty

    | (None, None) ->
      kinds, Subst.empty

    | (Some kindl, Some kindr) ->
      raise @@ Error (MismatchedKinds (kindl, kindr))
  in

  printf "%skinds: %s\n" indent (Map.sexp_of_m__t (module String) (sexp_of_kind) kinds |> Sexp.to_string);

  kinds, subst

(*
 * type solving
 *)

let solve_exn expr =
  let ctx = Ctx.create () in
  let env = Env.empty in
  let typ = generate_typ ctx env expr in

  print_endline "typ:";
  Types.print typ;
  print_endline "";

  let typ_constraints = Ctx.typ_constraints ctx in
  let kind_constraints = Ctx.kind_constraints ctx in

  print_endline "typ constraints:";
  typ_constraints
  |> List.map ~f:(fun (typ, typ') -> [typ; typ'])
  |> List.sexp_of_t (List.sexp_of_t sexp_of_typ)
  |> Sexp.to_string_hum
  |> print_endline;
  print_endline "";

  print_endline "kind constraints:";
  kind_constraints
  |> List.map ~f:(fun (name, kind) -> Sexp.List [sexp_of_string name; sexp_of_kind kind])
  |> List.sexp_of_t Sexp.sexp_of_t
  |> Sexp.to_string_hum
  |> print_endline;
  print_endline "";

  (* fold up the substitutions from unification of the constraints *)
  let kinds = Map.of_alist_exn (module String) kind_constraints in
  let kinds, subst = List.fold typ_constraints
      ~init:(kinds, Subst.empty)
      ~f:(fun (kinds, subst) (left, right) ->
          let left = Subst.apply_typ subst left in
          let right = Subst.apply_typ subst right in
          let kinds', subst' = unify_typs_exn kinds left right in
          print_endline "";
          kinds', Subst.merge subst' subst)
  in

  print_endline "subst:";
  Subst.print subst;
  print_endline "";

  let kinds = Map.map kinds ~f:(Subst.apply_kind subst) in
  let typ = Subst.apply_typ subst typ in

  match Map.data kinds |> List.find ~f:Types.invalid_kind with
  | Some kind -> raise @@ Error (InvalidKind kind)
  | None -> (typ, kinds)

let solve expr =
  try Ok (solve_exn expr) with
  | Error error -> Error error
