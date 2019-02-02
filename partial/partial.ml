open Ofluxl_std
open Ofluxl_syntax


let all es ~f =
  List.fold es ~init:true ~f:(fun v e -> v && f e)

let rec fully_known scope = function
  | Ast.Ident name ->
    Set.mem scope name

  | Integer _
  | Float _
  | Duration _
  | Time _
  | Regex _
  | Char _
  | String _ ->
    true

  | Plus (left, right)
  | Minus (left, right) 
  | Times (left, right) 
  | Div (left, right) -> 
    fully_known scope left
    && fully_known scope right

  | Uminus expr ->
    fully_known scope expr

  | Func (args, body, ret) ->
    let rec helper scope = function
      | Ast.Assign (name, expr) :: tail ->
        if fully_known scope expr then
          let scope = Set.add scope name in
          helper scope tail
        else
          scope, false

      | Expr expr :: tail -> 
        if fully_known scope expr then
          let scope, tail_known = helper scope tail in
          scope, tail_known
        else
          scope, false

      | [] -> 
        scope, true
    in

    let scope = 
      args
      |> List.map ~f:(fun (name, _) -> name)
      |> Set.of_list (module String)
      |> Set.union scope
    in

    let scope, known = helper scope body in
    known
    && fully_known scope ret

  | Call (expr, args) ->
    fully_known scope expr
    && all args ~f:(fun (_, expr) -> fully_known scope expr)

  | Pipe (left, right) ->
    fully_known scope left
    && fully_known scope right

  | List exprs ->
    all exprs ~f:(fully_known scope)

  | Record fields ->
    all fields ~f:(fun (_, expr) -> fully_known scope expr)

  | With (expr, fields) ->
    fully_known scope expr
    && all fields ~f:(fun (_, expr) -> fully_known scope expr)

  | Select (expr, _) ->
    fully_known scope expr

  | Index (expr, i) ->
    fully_known scope expr
    && fully_known scope i

  | Comp (left, _, right)
  | And (left, right)
  | Or (left, right) ->
    fully_known scope left
    && fully_known scope right

  | Return expr ->
    fully_known scope expr