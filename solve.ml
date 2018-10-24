open Std
open Types

module Ctx = struct
  type t =
    { mutable typ_constraints : (typ * typ) list
    ; mutable kind_constraints : (typ * kind) list
    ; mutable num: int
    }
  [@@deriving sexp]

  let create () =
    { typ_constraints = []
    ; kind_constraints = []
    ; num = 0
    }

  let typ_constraints { typ_constraints; _ } = typ_constraints
  let kind_constraints { kind_constraints; _ } = kind_constraints

  let add_typ_constraint ctx (left, right) =
    ctx.typ_constraints <- (left, right) :: ctx.typ_constraints

  let add_kind_constraint ctx (typ, kind) =
    ctx.kind_constraints <- (typ, kind) :: ctx.kind_constraints

  let fresh_type_name ctx =
    ctx.num <- ctx.num + 1;
    Printf.sprintf "a%d" ctx.num

  let fresh_type ctx =
    Variable (fresh_type_name ctx)
end

module Env = struct
  type t = typ Map.M(String).t
  [@@deriving sexp]

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
    Map.find_exn env name

  let print env = env |> sexp_of_t |> Sexp.to_string_hum |> print_endline
end

module Subst = struct
  type t = typ Map.M(String).t

  let empty = Map.empty (module String)

  let singleton = Map.singleton (module String)

  let merge left right =
    Map.merge left right ~f:(fun ~key:_ -> function
        | `Both (_, right) -> Some right
        | `Left left -> Some left
        | `Right right -> Some right
      )

  let rec apply subst = function
    | Variable name as typ -> begin
        match Map.find subst name with
        | Some typ -> typ
        | None -> typ
      end
    | List typ -> List (apply subst typ)
    | Func { args; table; required; ret } ->
      Func { args = Map.map args ~f:(apply subst)
           ; table
           ; required
           ; ret = apply subst ret
           }
    | _ as typ -> typ
end

let rec generate ctx env = function
  (* identifier lookup *)
  | Ast.Ident ident -> begin
      try Env.find env ident with
      | exn ->
        Env.print env;
        raise exn
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
    let typ = generate ctx env expr in
    Ctx.add_kind_constraint ctx (typ, Cls Num);
    typ

  | Ast.Plus (left, right) ->
    let typl = generate ctx env left in
    let typr = generate ctx env right in
    Ctx.add_typ_constraint ctx (typl, typr);
    Ctx.add_kind_constraint ctx (typl, Cls Add);
    Ctx.add_kind_constraint ctx (typr, Cls Add);
    typl

  | Ast.Minus (left, right)
  | Ast.Times (left, right)
  | Ast.Div (left, right) ->
    let typl = generate ctx env left in
    let typr = generate ctx env right in
    Ctx.add_typ_constraint ctx (typl, typr);
    Ctx.add_kind_constraint ctx (typl, Cls Num);
    Ctx.add_kind_constraint ctx (typr, Cls Num);
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
    let ret = Ctx.fresh_type ctx in
    (* TODO: can you call a function with a `in=<-` style argument somehow? *)
    Ctx.add_typ_constraint ctx (typl, Func { args; table = false; required; ret });
    ret

  | Ast.Pipe (left, Ast.Call (right, args)) ->
    let typl = generate ctx env left in
    let typr = generate ctx env right in
    let args, required = generate_call_args ctx env args in
    let ret = Ctx.fresh_type ctx in
    Ctx.add_typ_constraint ctx (typl, Basic Table);
    Ctx.add_typ_constraint ctx (typr, Func { args; table = true; required; ret });
    ret

  | Ast.Pipe _ as expr ->
    (* if the right side of the pipe isn't a call, it's a problem *)
    raise (Ast.Invalid expr)

  | Ast.Return expr -> generate ctx env expr

  (* composite types *)
  | Ast.List exprs ->
    let typ = Ctx.fresh_type ctx in
    let typs = List.map exprs ~f:(generate ctx env) in
    (* for now, assume lists are homogenous (??) *)
    List.iter typs ~f:(fun typ' -> Ctx.add_typ_constraint ctx (typ, typ'));
    typ

  | Ast.Record fields ->
    let typ = Ctx.fresh_type ctx in
    let fields, upper = generate_call_args ctx env fields in
    Ctx.add_kind_constraint ctx
      (typ, Record { fields
                   ; lower = Set.empty (module String)
                   ; upper = Some upper
                   });
    typ

  (* projections *)
  | Ast.Select (expr, field) ->
    let typ = Ctx.fresh_type ctx in
    let typr = generate ctx env expr in
    Ctx.add_kind_constraint ctx
      (typr, Record { fields = Map.singleton (module String) field typ
                    ; lower = Set.singleton (module String) field
                    ; upper = None (* universe *)
                    });
    typ

  | Ast.Index (expr, index) ->
    let typ = Ctx.fresh_type ctx in
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
    let typl = generate ctx env left in
    let typr = generate ctx env right in
    Ctx.add_kind_constraint ctx (typl, Cls Cmp);
    Ctx.add_kind_constraint ctx (typr, Cls Cmp);
    Ctx.add_typ_constraint ctx (typl, typr);
    Basic Bool

and generate_default ctx env = function
  | Some (Ast.DExpr expr) -> generate ctx env expr
  | Some Ast.DPipe -> Basic Table
  | None -> Ctx.fresh_type ctx

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

let try_unify ctx =
  let rec try_unify_typ = function
    | (typ, Variable name) -> bind name typ
    | (Variable name, typ) -> bind name typ
    (* most of these just need to check that they're physically the same *)
    | _ -> Error `Todo

  and bind name typ =
    if phys_equal (Variable name) typ then Ok Subst.empty
    else if Types.occurs name typ then Error (`Infinite (name, typ))
    else Ok (Subst.singleton name typ)

  (*
  and merge_kind_record
      (Record { fields = fieldsl; upper = upperl; lower = lowerl })
      (Record { fields = fieldsr; upper = upperr; lower = lowerr }) =

    let fields = Map.merge fieldsl fieldsr ~f:(fun ~key:_ -> function
        | `Left left -> Some left
        | `Right right -> Some right
        | `Both (left, right) ->
          match try_unify_typ (left, right) with
          | Ok subst ->  Some (Subst.apply subst left)
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

    { fields; upper; lower }
  *)

  and try_merge_kind = function
    | (Record { fields = fieldsl; upper = upperl; lower = lowerl },
       Record { fields = fieldsr; upper = upperr; lower = lowerr }) ->

      let fields = Map.merge fieldsl fieldsr ~f:(fun ~key:_ -> function
          | `Left left -> Some left
          | `Right right -> Some right
          | `Both (left, right) ->
            match try_unify_typ (left, right) with
            | Ok subst ->  Some (Subst.apply subst left)
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

      Ok (Record { fields; upper; lower })

    | (Cls Cmp, Cls Cmp) -> Ok (Cls Cmp)
    | (Cls Cmp, Cls Add) -> Ok (Cls Add)
    | (Cls Cmp, Cls Num) -> Ok (Cls Num)

    | _ -> Error `Todo
  in

  (* ignore these issues for now *)
  let _ = (try_unify_typ, bind, try_merge_kind, ctx) in
  Ok ()

exception Unification of
    [ `Infinite of (tvar * typ)
    | `Todo
    ]

let unify ctx =
  match try_unify ctx with
  | Ok subst -> subst
  | Error err -> raise @@ Unification err

let solve expr =
  let ctx = Ctx.create () in
  let env = Env.empty in
  let typ = generate ctx env expr in
  (typ, Ctx.typ_constraints ctx, Ctx.kind_constraints ctx)
