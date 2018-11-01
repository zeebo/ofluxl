open Ofluxl_std
open Ofluxl_types
open Ofluxl_syntax.Ast

let rec generate (ctx: Context.t) = function
  (*
   * identifier lookup
   *)
  | Ident ident -> ctx#resolve ident

  (*
   * basic types
   *)
  | Integer _  -> Type.wrap @@ Basic Integer
  | Float _    -> Type.wrap @@ Basic Float
  | Duration _ -> Type.wrap @@ Basic Duration
  | Time _     -> Type.wrap @@ Basic Time
  | Regex _    -> Type.wrap @@ Basic Regex
  | Char _     -> Type.wrap @@ Basic Char
  | String _   -> Type.wrap @@ Basic String

  (*
   * math expressions
   *)
  | Uminus expr ->
    let typ = generate ctx expr in
    ctx#kind_constraint typ @@ Cls Num;
    typ

  | Plus (left, right) ->
    let typl = generate ctx left in
    let typr = generate ctx right in
    ctx#typ_constraint typl typr;
    ctx#kind_constraint typl @@ Cls Add;
    ctx#kind_constraint typr @@ Cls Add;
    Type.refresh typl

  | Minus (left, right)
  | Times (left, right)
  | Div (left, right) ->
    let typl = generate ctx left in
    let typr = generate ctx right in
    ctx#typ_constraint typl typr;
    ctx#kind_constraint typl @@ Cls Num;
    ctx#kind_constraint typr @@ Cls Num;
    Type.refresh typl

  (*
   * logical operations
   *)
  | And (left, right)
  | Or (left, right) ->
    let typl = generate ctx left in
    let typr = generate ctx right in
    ctx#typ_constraint typl (Type.wrap @@ Basic Bool);
    ctx#typ_constraint typr (Type.wrap @@ Basic Bool);
    Type.wrap @@ Basic Bool

  (*
   * function support
   *)
  | Func (args, body) ->
    let args, table, required = generate_func_args ctx args in
    let entries = Map.map args ~f:Scheme.empty in
    let ret = ctx#scope entries (fun ctx -> generate ctx body) in
    Type.wrap @@ Func { args; table; required; ret }

  | Call (expr, args) ->
    let typl = generate ctx expr in
    let args, required = generate_call_args ctx args in
    let ret = ctx#fresh_variable in
    ctx#typ_constraint typl (Type.wrap @@ Func { args; table = false; required; ret });
    ret

  | Pipe (left, Call (right, args)) ->
    let typl = generate ctx left in
    let typr = generate ctx right in
    let args, required = generate_call_args ctx args in
    let ret = ctx#fresh_variable in
    ctx#typ_constraint typl (Type.wrap @@ Basic Table);
    ctx#typ_constraint typr (Type.wrap @@ Func { args; table = true; required; ret });
    ret

  | Pipe _ as expr -> (* if the right side of the pipe isn't a call, it's a problem *)
    raise (Invalid expr)

  | Return expr ->
    generate ctx expr

  (*
   * composite types
   *)
  | List exprs ->
    let typ = ctx#fresh_variable in
    let typs = List.map exprs ~f:(generate ctx) in
    List.iter typs ~f:(ctx#typ_constraint typ);
    Type.wrap @@ List typ

  | Record fields ->
    let typ = ctx#fresh_variable in
    let fields, upper = generate_call_args ctx fields in
    let kind = Kind.Record
        { fields
        ; lower = Set.empty (module String)
        ; upper = Some upper
        } in
    ctx#kind_constraint typ kind;
    typ

  (*
   * projections
   *)
  | Select (expr, field) ->
    let typf = ctx#fresh_variable in
    let typ = generate ctx expr in
    let kind = Kind.Record
        { fields = Map.singleton (module String) field typf
        ; lower = Set.singleton (module String) field
        ; upper = None (* universe *)
        } in
    ctx#kind_constraint typ kind;
    typf

  | Index (expr, index) ->
    let typ = ctx#fresh_variable in
    let typl = generate ctx expr in
    let typi = generate ctx index in
    ctx#typ_constraint typl (Type.wrap @@ List typ);
    ctx#typ_constraint typi (Type.wrap @@ Basic Integer);
    typ

  (*
   * comparisons
   *)
  | Comp (left, _, right) ->
    let typl = generate ctx left in
    let typr = generate ctx right in
    ctx#typ_constraint typl typr;
    ctx#kind_constraint typl @@ Cls Cmp;
    ctx#kind_constraint typr @@ Cls Cmp;
    Type.wrap @@ Basic Bool

(*
 * helpers
 *)

(* generate a type for a given default argument *)
and generate_default ctx = function
  | Some (DExpr expr) -> generate ctx expr
  | Some DPipe -> Type.wrap @@ Basic Table
  | None -> ctx#fresh_variable

(* generate the information for a function *)
and generate_func_args ctx args =
  let table =
    List.exists args ~f:(function
        | _, Some DPipe -> true
        | _ -> false)
  in

  let required =
    Set.of_list (module String) @@
    List.filter_map args ~f:(function
        | _, Some _ -> None
        | name, None -> Some name)
  in

  let args =
    Map.of_alist_exn (module String) @@
    List.map args ~f:(fun (name, def) -> (name, generate_default ctx def))
  in

  (args, table, required)

(* generate the information for a call *)
and generate_call_args ctx args =
  let args =
    Map.of_alist_exn (module String) @@
    List.map args ~f:(fun (name, expr) -> (name, generate ctx expr))
  in

  (args, Set.of_list (module String) @@ Map.keys args)
