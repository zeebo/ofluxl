open Std
open Types

type error =
  | Infinite of (tvar * typ)
  | MismatchedKinds of (kind * kind)
  | MismatchedTypes of (typ * typ)
  | InvalidSubst of (string * typ)
  | UnknownIdentifier of string

exception Error of error

module Ctx = struct
  type t =
    { mutable typ_constraints : (typ * typ) list
    ; mutable kind_constraints : (string * kind) list
    ; mutable num: int
    }
  [@@deriving sexp]

  let print ctx = print_endline @@ Sexp.to_string_hum @@ sexp_of_t ctx

  let create () =
    { typ_constraints = []
    ; kind_constraints = []
    ; num = 0
    }

  let typ_constraints { typ_constraints; _ } = typ_constraints
  let kind_constraints { kind_constraints; _ } = kind_constraints

  let add_typ_constraint ctx (left, right) =
    ctx.typ_constraints <- (left, right) :: ctx.typ_constraints

  let add_kind_constraint ctx (name, kind) =
    ctx.kind_constraints <- (name, kind) :: ctx.kind_constraints

  let fresh_type_name ctx =
    ctx.num <- ctx.num + 1;
    Printf.sprintf "a%d" ctx.num
end

module Env = struct
  type t = typ Map.M(String).t
  [@@deriving sexp]

  let print ctx = print_endline @@ Sexp.to_string_hum @@ sexp_of_t ctx

  let empty = Map.of_alist_exn (module String)
      [ ("true", Basic Bool)
      ; ("false", Basic Bool)
      ]

  let insert env args =
    Map.merge env args ~f:(fun ~key:_ -> function
        | `Both (_, right) -> Some right
        | `Left left -> Some left
        | `Right right -> Some right
      )

  let find env name =
    Map.find env name
end

module Subst = struct
  type t = typ Map.M(String).t
  [@@deriving sexp]

  let print ctx = print_endline @@ Sexp.to_string_hum @@ sexp_of_t ctx

  let empty = Map.empty (module String)

  let singleton = Map.singleton (module String)

  let rec subst_typ name typ = function
    | Variable name' as typ' ->
      if String.equal name' name then typ else typ'
    | Record name' as typ' ->
      if String.equal name' name then typ else typ'
    | List typ' -> List (subst_typ name typ typ')
    | Func { args; table; required; ret } ->
      Func { args = Map.map args ~f:(subst_typ name typ)
           ; table
           ; required
           ; ret = subst_typ name typ ret
           }
    | Basic _ as typ -> typ
    | Invalid as typ -> typ

  let subst_kind name typ = function
    | (name', (KCls _ as kind)) ->
      let name' =
        match (String.equal name' name, typ) with
        | (true, Variable name) -> name
        | _ -> name'
      in

      (name', kind)

    | (name', KRecord { fields; upper; lower }) ->
      let name' =
        match (String.equal name' name, typ) with
        | (true, Record name) -> name
        | _ -> name'
      in

      ( name'
      , KRecord { fields = Map.map fields ~f:(subst_typ name typ)
                ; upper
                ; lower
                }
      )

  let apply_typ subst typ =
    Map.fold subst ~init:typ ~f:(fun ~key ~data typ ->
        subst_typ key data typ)

  let apply_kind subst kind =
    Map.fold subst ~init:kind ~f:(fun ~key ~data kind ->
        subst_kind key data kind)

  let merge substl substr unify =
    Map.merge substl substr ~f:(fun ~key:_ -> function
        | `Both (left, right) ->
          let subst = unify left right in
          Some (apply_typ subst left)
        | `Left left -> Some (apply_typ substr left)
        | `Right right -> Some (apply_typ substl right)
      )
end

let rec generate ctx env = function
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
    let typ = generate ctx env expr in
    Ctx.add_kind_constraint ctx (name, KCls Num);
    Ctx.add_typ_constraint ctx (Variable name, typ);
    typ

  | Ast.Plus (left, right) ->
    let namel = Ctx.fresh_type_name ctx in
    let typl = generate ctx env left in
    let namer = Ctx.fresh_type_name ctx in
    let typr = generate ctx env right in
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
    let typl = generate ctx env left in
    let namer = Ctx.fresh_type_name ctx in
    let typr = generate ctx env right in
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
    let typl = generate ctx env left in
    let typr = generate ctx env right in
    Ctx.add_typ_constraint ctx (typl, Basic Bool);
    Ctx.add_typ_constraint ctx (typr, Basic Bool);
    Basic Bool

  (* function support *)
  | Ast.Func (args, body) ->
    let args, table, required = generate_func_args ctx env args in
    let env' = Env.insert env args in
    let ret = generate ctx env' body in
    Func { args; table; required; ret }

  | Ast.Call (expr, args) ->
    let typl = generate ctx env expr in
    let args, required = generate_call_args ctx env args in
    let ret = Variable (Ctx.fresh_type_name ctx) in
    (* TODO: can you call a function with a `in=<-` style argument somehow? *)
    Ctx.add_typ_constraint ctx (typl, Func { args; table = false; required; ret });
    ret

  | Ast.Pipe (left, Ast.Call (right, args)) ->
    let typl = generate ctx env left in
    let typr = generate ctx env right in
    let args, required = generate_call_args ctx env args in
    let ret = Variable (Ctx.fresh_type_name ctx) in
    Ctx.add_typ_constraint ctx (typl, Basic Table);
    Ctx.add_typ_constraint ctx (typr, Func { args; table = true; required; ret });
    ret

  | Ast.Pipe _ as expr ->
    (* if the right side of the pipe isn't a call, it's a problem *)
    raise (Ast.Invalid expr)

  | Ast.Return expr -> generate ctx env expr

  (* composite types *)
  | Ast.List exprs ->
    let typ = Variable (Ctx.fresh_type_name ctx) in
    let typs = List.map exprs ~f:(generate ctx env) in
    (* for now, assume lists are homogenous (??) *)
    List.iter typs ~f:(fun typ' -> Ctx.add_typ_constraint ctx (typ, typ'));
    typ

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
    let typ = generate ctx env expr in
    Ctx.add_typ_constraint ctx (Record name, typ);
    Ctx.add_kind_constraint ctx
      (name, KRecord { fields = Map.singleton (module String) field typf
                     ; lower = Set.singleton (module String) field
                     ; upper = None (* universe *)
                     });
    typf

  | Ast.Index (expr, index) ->
    let typ = Variable (Ctx.fresh_type_name ctx) in
    let typl = generate ctx env expr in
    let typi = generate ctx env index in
    Ctx.add_typ_constraint ctx (typl, List typ);
    Ctx.add_typ_constraint ctx (typi, Basic Integer);
    typ

  (* assignment *)
  | Ast.Assign (_name, expr) ->
    let typ = generate ctx env expr in
    (* TODO: we need to have an idea of what statements come
     * after in order for assignment to make any sense. for
     * now assume assignment returns the value that was
     * assigned.
    *)
    typ

  (* comparisons *)
  | Ast.Comp (left, _, right) ->
    let namel = Ctx.fresh_type_name ctx in
    let typl = generate ctx env left in
    let namer = Ctx.fresh_type_name ctx in
    let typr = generate ctx env right in
    Ctx.add_typ_constraint ctx (Variable namel, typl);
    Ctx.add_kind_constraint ctx (namel, KCls Cmp);
    Ctx.add_typ_constraint ctx (Variable namer, typr);
    Ctx.add_kind_constraint ctx (namer, KCls Cmp);
    Ctx.add_typ_constraint ctx (typl, typr);
    Basic Bool

and generate_default ctx env = function
  | Some (Ast.DExpr expr) -> generate ctx env expr
  | Some Ast.DPipe -> Basic Table
  | None -> Variable (Ctx.fresh_type_name ctx)

and generate_func_args ctx env args =
  let table = args
              |> List.exists ~f:(fun (_, def) -> match def with
                  | Some Ast.DPipe -> true
                  | _ -> false)
  in
  let required = args
                 |> List.filter_map ~f:(fun (name, def) ->
                     match def with
                     | Some _ -> None
                     | None -> Some name)
                 |> Set.of_list (module String)
  in
  let args = args
             |> List.map ~f:(fun (name, def) -> (name, generate_default ctx env def))
             |> Map.of_alist_exn (module String)
  in
  ( args, table, required)

and generate_call_args ctx env args =
  let args = args
             |> List.map ~f:(fun (name, expr) -> (name, generate ctx env expr))
             |> Map.of_alist_exn (module String)
  in
  ( args, Set.of_list (module String) @@ Map.keys args )

let solve_exn expr =
  let rec unify_typs_exn left right =
    match (left, right) with
    (* unify records *)
    | (Record namel, Record namer) ->
      Subst.singleton namel (Record namer)

    (* variables unify with each other *)
    | (Variable namel, Variable namer) ->
      Subst.singleton namel (Variable namer)

    (* a variable with anything else substitutes
     * unless that would cause an infinite type *)
    | (Variable name, typ)
    | (typ, Variable name) ->
      if Types.occurs name typ then
        raise @@ Error (Infinite (name, typ))
      else
        Subst.singleton name typ

    (* lists must unify their element types *)
    | (List left, List right) -> unify_typs_exn left right

    (* functions must have the same argument types *)
    | (Func {args = argsl; table = tablel; required = requiredl; ret = retl },
       Func {args = argsr; table = tabler; required = requiredr; ret = retr}) as typs ->

      let namesl = List.sort ~compare:String.compare @@ Map.keys argsl in
      let namesr = List.sort ~compare:String.compare @@ Map.keys argsr in

      if not @@ Bool.equal tablel tabler ||
         not @@ List.equal namesl namesr ~equal:String.equal ||
         not @@ Set.is_subset requiredl ~of_:requiredr
      then
        raise @@ Error (MismatchedTypes typs)
      else
        let init = unify_typs_exn retl retr in
        List.fold namesl ~init ~f:(fun subst key ->
            let typl = Map.find_exn argsl key in
            let typr = Map.find_exn argsr key in
            let subst' = unify_typs_exn typl typr in
            Subst.merge subst subst' unify_typs_exn
          )

    (* everything else has to be exactly equal and does not require subs *)
    | (left, right) as typs ->
      if phys_equal left right then
        Subst.empty
      else
        raise @@ Error (MismatchedTypes typs)

  and merge_kind left right =
    match (left, right) with
    | (KRecord { fields = fieldsl; upper = upperl; lower = lowerl },
       KRecord { fields = fieldsr; upper = upperr; lower = lowerr }) ->

      let fields = Map.merge fieldsl fieldsr ~f:(fun ~key:_ -> function
          | `Left left -> Some left
          | `Right right -> Some right
          | `Both (left, right) ->
            try
              let subst = unify_typs_exn left right in
              Some (Subst.apply_typ subst left)
            with
            | Error _ -> Some Invalid
        )
      in
      let upper = match (upperl, upperr) with
        | (None, Some upperr) -> Some upperr
        | (Some upperl, None) -> Some upperl
        | (None, None) -> None
        | (Some upperl, Some upperr) -> Some (Set.union upperl upperr)
      in
      let lower = Set.inter lowerl lowerr in

      KRecord { fields; upper; lower }

    | (KCls Cmp, KCls Cmp) -> KCls Cmp
    | (KCls Cmp, KCls Add) -> KCls Add
    | (KCls Cmp, KCls Num) -> KCls Num

    | (KCls Add, KCls Cmp) -> KCls Add
    | (KCls Add, KCls Add) -> KCls Add
    | (KCls Add, KCls Num) -> KCls Num

    | (KCls Num, KCls Cmp) -> KCls Num
    | (KCls Num, KCls Add) -> KCls Num
    | (KCls Num, KCls Num) -> KCls Num

    | kinds -> raise @@ Error (MismatchedKinds kinds)
  in

  let ctx = Ctx.create () in
  let env = Env.empty in
  let typ = generate ctx env expr in

  print_endline "typ:";
  print_endline @@ Sexp.to_string_hum @@ Types.sexp_of_typ typ;
  print_endline "";

  print_endline "ctx:";
  Ctx.print ctx;
  print_endline "";

  let typ_constraints = Ctx.typ_constraints ctx in
  let subst = List.fold typ_constraints
      ~init:Subst.empty
      ~f:(fun subst (left, right) ->
          Subst.merge subst (unify_typs_exn left right) unify_typs_exn)
  in

  print_endline "subst:";
  Subst.print subst;
  print_endline "";

  let kind_constraints = Ctx.kind_constraints ctx in
  let kind_constraints = List.map kind_constraints
      ~f:(Subst.apply_kind subst)
  in
  let kind = Map.of_alist_reduce (module String) kind_constraints
      ~f:merge_kind
  in

  (Subst.apply_typ subst typ, kind)

let solve expr =
  try Ok (solve_exn expr) with
  | Error error -> Error error
