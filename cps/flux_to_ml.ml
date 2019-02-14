open Ofluxl_std

open Tvar
open Ml
open Ofluxl_syntax

exception Unimplemented

module Env = struct
  type t

  let set env n e =
    Map.set env ~key:n ~data:e

  let find env n =
    Map.find_exn env n

  let of_alist args =
    Map.of_alist_exn (module String) args

  let merge env ~into =
    Map.merge env into ~f:(fun ~key:_ -> function
        | `Both (v, _) -> Some v
        | `Left v -> Some v
        | `Right v -> Some v)

  let empty =
    Map.empty (module String)
end

let builtin name =
  Var (Builtins.get name)

let rec conv env = function
  | [] -> Prim Prim.Void
  | (None, e) :: tail ->
    Let (Var.fresh (), conv_expr env e, conv env tail)
  | (Some n, e) :: tail ->
    let x = Var.fresh () in
    let e = conv_expr env e in
    Let (x, e, conv (Env.set env n x) tail)

and conv_expr env = function
  | Ast.Ident name -> Var (Env.find env name)

  | Integer i -> Prim (Prim.Integer i)
  | Float f -> Prim (Prim.Float f)
  | Duration d -> Prim (Prim.Duration d)
  | Time t -> Prim (Prim.Time t)
  | Regex r -> Prim (Prim.Regex r)
  | Char c -> Prim (Prim.Char c)
  | Bool b -> Prim (Prim.Bool b)
  | String s -> Prim (Prim.String s)

  | Plus (e1, e2) -> App (builtin "add", Tuple [conv_expr env e1; conv_expr env e2])
  | Minus (e1, e2) -> App (builtin "sub", Tuple [conv_expr env e1; conv_expr env e2])
  | Times (e1, e2) -> App (builtin "mul", Tuple [conv_expr env e1; conv_expr env e2])
  | Div (e1, e2) -> App (builtin "div", Tuple [conv_expr env e1; conv_expr env e2])
  | Uminus e -> App (builtin "neg", conv_expr env e)

  | Func (args, body, ret) ->
    (* assign each argument a numeric index *)
    let args =
      args
      |> List.sort ~compare:(fun (n1, _) (n2, _) -> String.compare n1 n2)
      |> List.map ~f:(fun (n, _) -> (n, Var.fresh ()))
    in

    (* create the env to run the body conversion in *)
    let env' = Env.merge (Env.of_alist args) ~into:env
    in

    (* convert the body, updating the env *)
    let rec helper env = function
      | [] -> conv_expr env ret
      | (None, e) :: tail ->
        Let (Var.fresh (), conv_expr env e, helper env tail)
      | (Some n, e) :: tail ->
        let x = Var.fresh () in
        let e = conv_expr env e in
        Let (x, e, helper (Env.set env n x) tail)
    in

    let body = helper env' body
    in

    (* generate an argument variable for the lambda *)
    let arg = Var.fresh ()
    in

    (* convert the arguments to a sequence of lets *)
    let rec helper i = function
      | [] -> body
      | (_, x) :: tail ->
        Let (x, Pi (i, Var arg), helper (i + 1) tail)
    in

    (* return the lambda *)
    Lam (arg, helper 0 args)

  | Call (func, args) ->
    (* TODO(jeff): i am 99% sure that this is problematic even after the type
       checker. like defaults need to happen, somewhere. we need the type info.
    *)
    let args =
      args
      |> List.sort ~compare:(fun (n1, _) (n2, _) -> String.compare n1 n2)
      |> List.map ~f:(fun (_, e) -> conv_expr env e)
    in
    App (conv_expr env func, Tuple args)

  | Pipe (left, right) ->
    (* TODO(jeff): this is way wrong. we need the type information. *)
    let left = conv_expr env left in
    let right = conv_expr env right in
    App (left, right)

  | List exprs ->
    Tuple (List.map ~f:(conv_expr env) exprs)

  | Record fields ->
    fields
    |> List.sort ~compare:(fun (n1, _) (n2, _) -> String.compare n1 n2)
    |> List.map ~f:(fun (_, e) -> conv_expr env e)
    |> fun x -> Tuple x

  | Select (expr, _field) ->
    (* TODO(jeff): we need the type information. *)
    Pi (0, conv_expr env expr)

  | Index (expr, Integer n) ->
    Pi (Int.of_string n, conv_expr env expr)

  | Index (expr, index) ->
    App (builtin "sel", Tuple [conv_expr env expr; conv_expr env index])

  | Comp (left, cmp, right) ->
    (* TODO(jeff): have builtins based on the comparison? *)
    App (builtin "cmp", Tuple [conv_expr env left; Prim (Prim.String cmp); conv_expr env right])

  | And (left, right) ->
    App (builtin "and", Tuple [conv_expr env left; conv_expr env right])

  | Or (left, right) ->
    App (builtin "or", Tuple [conv_expr env left; conv_expr env right])

  | Ternary (cmp, left, right) ->
    App (builtin "tern", Tuple [conv_expr env cmp; conv_expr env left; conv_expr env right])
