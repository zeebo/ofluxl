open Ofluxl_std
open Ofluxl_syntax

let sym = object
  val mutable c = 0
  method next =
    c <- c + 1;
    sprintf ".expr%d" c
end

let scope_keys scope =
  Set.of_list (module String) @@ Map.keys scope

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
  | Bool _
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

let rec peval_expr scope = function
  | Ast.Ident name as expr ->
    begin match Map.find scope name with
      | None -> expr
      | Some expr -> expr
    end

  | Integer _
  | Float _
  | Duration _
  | Time _
  | Regex _
  | Char _
  | Bool _
  | String _ as expr ->
    expr

  | Plus (left, right) ->
    begin match peval_expr scope left, peval_expr scope right with
      | Integer l, Integer r -> Eval.add (module Eval.Integer) l r
      | Float l, Float r -> Eval.add (module Eval.Float) l r
      | String l, String r -> Eval.add (module Eval.String) l r
      | left, right -> Plus (left, right)
    end

  | Minus (left, right) ->
    begin match peval_expr scope left, peval_expr scope right with
      | Integer l, Integer r -> Eval.sub (module Eval.Integer) l r
      | Float l, Float r -> Eval.sub (module Eval.Float) l r
      | left, right -> Minus (left, right)
    end

  | Times (left, right) ->
    begin match peval_expr scope left, peval_expr scope right with
      | Integer l, Integer r -> Eval.mul (module Eval.Integer) l r
      | Float l, Float r -> Eval.mul (module Eval.Float) l r
      | left, right -> Minus (left, right)
    end

  | Div (left, right) ->
    begin match peval_expr scope left, peval_expr scope right with
      | Integer l, Integer r -> Eval.div (module Eval.Integer) l r
      | Float l, Float r -> Eval.div (module Eval.Float) l r
      | left, right -> Minus (left, right)
    end

  | Uminus expr ->
    begin match peval_expr scope expr with
      | Integer v -> Eval.neg (module Eval.Integer) v
      | Float v -> Eval.neg (module Eval.Float) v
      | expr -> Uminus expr
    end

  | Func (args, body, ret) ->
    let body = List.map body ~f:(function
        | Ast.Assign (name, expr) -> Ast.Assign (name, peval_expr scope expr)
        | Ast.Expr expr -> Ast.Expr (peval_expr scope expr))
    in
    Func (args, body, peval_expr scope ret)

  | Call (func, args) ->
    let args = List.map args ~f:(fun (name, expr) ->
        name, peval_expr scope expr)
    in

    begin match peval_expr scope func with
      | Func (params, body, ret) as func ->
        (* ensure we have all the arguments necessary for non-default values *)
        let total =
          List.map params ~f:(fun (name, _) -> name)
          |> Set.of_list (module String)

        and required =
          List.filter_map params ~f:(function
              | name, None -> Some name
              | _ -> None)
          |> Set.of_list (module String)

        and provided =
          List.map args ~f:(fun (name, _) -> name)
          |> Set.of_list (module String)
        in

        if not @@ Set.is_subset required ~of_:provided ||
           not @@ Set.is_subset provided ~of_:total then
          Call (func, args)
        else

          (* set the defaults into a new scope *)
          let scope' =
            List.fold params ~init:scope ~f:(fun scope' -> function
                | name, Some (DExpr expr) ->
                  let expr = peval_expr scope expr in
                  Map.set scope' ~key:name ~data:expr
                | _ -> scope')
          in

          (* set the called arguments in that scope *)
          let scope' =
            List.fold args ~init:scope' ~f:(fun scope' (name, expr) ->
                let expr = peval_expr scope expr in
                Map.set scope' ~key:name ~data:expr)
          in

          (* evaluate the function body in the new scope *)
          let scope' =
            peval_statements scope' body
          in

          (* evaluate the return value in the context of that scope *)
          peval_expr scope' ret

      | func -> Call (func, args)
    end

  | Pipe (left, right) ->
    (* TODO(jeff): handling pipes *)
    Pipe (left, right)

  | List exprs ->
    List (List.map exprs ~f:(peval_expr scope))

  | Record fields ->
    Record (List.map fields ~f:(fun (name, expr) -> (name, peval_expr scope expr)))

  | Select (expr, field) ->
    begin match peval_expr scope expr with
      | Record fields as expr ->
        begin match List.Assoc.find fields ~equal:String.equal field with
          | Some expr -> peval_expr scope expr
          | None -> Select (expr, field)
        end
      | expr -> Select (expr, field)
    end

  | Index (expr, index) ->
    begin match peval_expr scope expr, peval_expr scope index with
      | List exprs as expr, (Integer n as index) ->
        begin match List.nth exprs (Int.of_string n) with
          | Some expr -> peval_expr scope expr
          | None -> Index (expr, index)
        end

      | Record fields as expr, (String field as index) ->
        begin match List.Assoc.find fields ~equal:String.equal field with
          | Some expr -> peval_expr scope expr
          | None -> Index (expr, index)
        end
      | expr, index -> Index (expr, index)
    end

  | Comp (left, cmp, right) as expr ->
    let fn = match cmp with
      | "==" -> Eval.eq
      | "!=" -> Eval.neq
      | "<" -> Eval.lt
      | ">" -> Eval.gt
      | "<=" -> Eval.leq
      | ">=" -> Eval.geq
      | _ -> fun (module E: Eval.Cmp) _ _ -> expr
    in
    begin match peval_expr scope left, peval_expr scope right with
      | Integer l, Integer r -> fn (module Eval.Integer) l r
      | Float l, Float r -> fn (module Eval.Float) l r
      | String l, String r -> fn (module Eval.String) l r
      | left, right -> Comp (left, cmp, right)
    end

  | And (left, right) ->
    begin match peval_expr scope left, peval_expr scope right with
      | Bool l, Bool r -> Bool (l && r)
      | left, right -> And (left, right)
    end

  | Or (left, right) ->
    begin match peval_expr scope left, peval_expr scope right with
      | Bool l, Bool r -> Bool (l && r)
      | left, right -> Or (left, right)
    end

  | Return expr ->
    peval_expr scope expr

and peval_statements scope = function
  | Ast.Assign (name, expr) :: tail ->
    let expr = peval_expr scope expr in
    let scope = Map.set scope ~key:name ~data:expr in
    peval_statements scope tail

  | Expr expr :: tail ->
    let expr = peval_expr scope expr in
    let scope = Map.set scope ~key:sym#next ~data:expr in
    peval_statements scope tail

  | [] ->
    scope

and peval_program program =
  program
  |> List.fold
    ~init:(Map.empty (module String), [])
    ~f:(fun (scope, acc) -> function
        | Ast.Assign (name, expr) ->
          let expr = peval_expr scope expr in
          let scope = Map.set scope ~key:name ~data:expr in
          if fully_known (scope_keys scope) expr
          then scope, acc
          else scope, Ast.Assign (name, expr) :: acc
        | Ast.Expr expr ->
          let expr = peval_expr scope expr in
          scope, Ast.Expr expr :: acc)
  |> snd
  |> List.rev
