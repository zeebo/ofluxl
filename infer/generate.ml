open Ofluxl_std
open Ofluxl_types
open Ofluxl_syntax.Ast

let rec generate (ctx: Context.t): expr -> Type.t = function
  (*
   * identifier lookup
   *)
  | Ident ident -> ctx#resolve ident

  (*
   * basic types
   *)
  | Integer _  -> Basic Integer
  | Float _    -> Basic Float
  | Duration _ -> Basic Duration
  | Time _     -> Basic Time
  | Regex _    -> Basic Regex
  | Char _     -> Basic Char
  | String _   -> Basic String

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
    typl

  | Minus (left, right)
  | Times (left, right)
  | Div (left, right) ->
    let typl = generate ctx left in
    let typr = generate ctx right in
    ctx#typ_constraint typl typr;
    ctx#kind_constraint typl @@ Cls Num;
    ctx#kind_constraint typr @@ Cls Num;
    typl

  (*
   * logical operations
   *)
  | And (left, right)
  | Or (left, right) ->
    let typl = generate ctx left in
    let typr = generate ctx right in
    ctx#typ_constraint typl @@ Basic Bool;
    ctx#typ_constraint typr @@ Basic Bool;
    Basic Bool

  (*
   * function support
   *)
  | Func (args, body, ret) ->
    let args, table, required = generate_func_args ctx args in
    let entries = Map.map args ~f:Scheme.empty in
    let ret = ctx#scope entries (fun ctx ->
        List.iter body ~f:(function
            | Expr expr -> ignore (generate ctx expr)
            | Assign (ident, expr) ->
              let typ = generate ctx expr in
              ctx#insert ident (ctx#generalize typ));
        generate ctx ret
      ) in
    Func { args; table; required; ret }

  | Call (expr, args) ->
    let typl = generate ctx expr in
    let args, required = generate_call_args ctx args in
    let ret = ctx#fresh_variable in
    ctx#typ_constraint typl @@ Func { args; table = false; required; ret };
    ret

  | Pipe (left, Call (right, args)) ->
    let typl = generate ctx left in
    let typr = generate ctx right in
    let args, required = generate_call_args ctx args in
    let ret = ctx#fresh_variable in
    ctx#typ_constraint typl @@ Basic Table;
    ctx#typ_constraint typr @@ Func { args; table = true; required; ret };
    ret

  | Pipe _ as expr -> (* if the right side of the pipe isn't a call, it's a problem *)
    raise @@ Invalid expr

  | Return expr ->
    generate ctx expr

  (*
   * composite types
   *)
  | List exprs ->
    let typ = ctx#fresh_variable in
    let typs = List.map exprs ~f:(generate ctx) in
    List.iter typs ~f:(ctx#typ_constraint typ);
    List typ

  | Record fields ->
    let typ = ctx#fresh_variable in
    let fields, lower, upper = generate_record_fields ctx fields in
    ctx#kind_constraint typ @@ Record { fields; lower; upper };
    typ

  (*
   * projections
   *)
  | Select (expr, field)
  | Index (expr, String field) ->
    let typf = ctx#fresh_variable in
    let typ = generate ctx expr in
    let fields = Map.singleton (module String) field typf in
    let lower = Set.singleton (module String) field in
    let upper = None (* universe *) in
    ctx#kind_constraint typ @@ Record { fields; lower; upper };
    typf

  | Index (expr, index) ->
    let typ = ctx#fresh_variable in
    let typl = generate ctx expr in
    let typi = generate ctx index in
    ctx#typ_constraint typl @@ List typ;
    ctx#typ_constraint typi @@ Basic Integer;
    typ

  (*
   * comparisons
   *)
  | Comp (left, "=~", right) ->
    let typl = generate ctx left in
    let typr = generate ctx right in
    ctx#typ_constraint typl @@ Basic String;
    ctx#typ_constraint typr @@ Basic Regex;
    Basic Bool

  | Comp (left, _, right) ->
    let typl = generate ctx left in
    let typr = generate ctx right in
    ctx#typ_constraint typl typr;
    ctx#kind_constraint typl @@ Cls Cmp;
    ctx#kind_constraint typr @@ Cls Cmp;
    Basic Bool

(*
 * helpers
 *)

(* generate a type for a given default argument *)
and generate_default ctx = function
  | Some (DExpr expr) -> generate ctx expr
  | Some DPipe -> Basic Table
  | None -> ctx#fresh_variable

(* generate the information for a function *)
and generate_func_args ctx args =
  let table = List.exists args ~f:(function
      | _, Some DPipe -> true
      | _ -> false)
  in

  let required = Set.of_list (module String) @@
    List.filter_map args ~f:(function
        | _, Some _ -> None
        | name, None -> Some name)
  in

  let args = Map.of_alist_exn (module String) @@
    List.map args ~f:(fun (name, def) -> (name, generate_default ctx def))
  in

  (args, table, required)

(* generate the information for a call *)
and generate_call_args ctx args =
  let args = Map.of_alist_exn (module String) @@
    List.map args ~f:(fun (name, expr) -> (name, generate ctx expr))
  in

  (args, Set.of_list (module String) @@ Map.keys args)

(* generate the information for record fields *)
and generate_record_fields ctx fields =
  let fields = Map.of_alist_exn (module String) @@
    List.map fields ~f:(fun (name, expr) -> (name, generate ctx expr))
  in

  (fields, Set.empty (module String), Some (Set.of_list (module String) @@ Map.keys fields))
