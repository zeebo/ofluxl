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
      | (Some name, expr) :: tail ->
        if fully_known scope expr then
          let scope = Set.add scope name in
          helper scope tail
        else
          scope, false

      | (None, expr) :: tail ->
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
      |> List.map ~f:fst
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

  | Ternary (cond, left, right) ->
    fully_known scope cond
    && fully_known scope left
    && fully_known scope right

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

  | Plus (Integer l, Integer r) -> Eval.add (module Eval.Integer) l r
  | Plus (Float l, Float r) -> Eval.add (module Eval.Float) l r
  | Plus (String l, String r) -> Eval.add (module Eval.String) l r
  | Plus (left, right) -> Plus (peval_expr scope left, peval_expr scope right)

  | Minus (Integer l, Integer r) -> Eval.sub (module Eval.Integer) l r
  | Minus (Float l, Float r) -> Eval.sub (module Eval.Float) l r
  | Minus (left, right) -> Minus (peval_expr scope left, peval_expr scope right)

  | Times (Integer l, Integer r) -> Eval.mul (module Eval.Integer) l r
  | Times (Float l, Float r) -> Eval.mul (module Eval.Float) l r
  | Times (left, right) -> Times (peval_expr scope left, peval_expr scope right)

  | Div (Integer l, Integer r) -> Eval.div (module Eval.Integer) l r
  | Div (Float l, Float r) -> Eval.div (module Eval.Float) l r
  | Div (left, right) -> Div (peval_expr scope left, peval_expr scope right)

  | Uminus (Integer expr) -> Eval.neg (module Eval.Integer) expr
  | Uminus (Float expr) -> Eval.neg (module Eval.Float) expr
  | Uminus expr -> Uminus (peval_expr scope expr)

  | Func (params, body, ret) ->
    Func (params, body, ret)

  | Call (Func (params, body, ret) as func, args) ->
    (* ensure we have all the arguments necessary for non-default values *)
    let total = List.map params ~f:fst |> Set.of_list (module String)
    and provided = List.map args ~f:fst |> Set.of_list (module String)
    and required =
      List.filter_map params ~f:(function
          | name, None -> Some name
          | name, Some Ast.DPipe -> Some name
          | _ -> None)
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
      let scope', _ =
        peval_statements scope' (Set.empty (module String)) body
      in

      (* evaluate the return value in the context of that scope *)
      (* we must fully evaluate it because it's in a different scope *)
      peval_fully_expr scope' ret

  | Call (expr, args) ->
    Call (peval_expr scope expr, args)

  | Pipe (left, Call (Func (params, _, _) as func, args)) as expr ->
    let is_pipe (_, def) = match def with | Some Ast.DPipe -> true | _ -> false in
    begin match List.find params ~f:is_pipe with
      | Some (name, _) ->
        Ast.Call (func, List.Assoc.add args ~equal:String.equal name left)
      | _ -> expr
    end

  | Pipe (left, Call (right, args)) ->
    Pipe (left, Call (peval_expr scope right, args))

  | Pipe (left, right) ->
    Pipe (left, peval_expr scope right)

  | List exprs ->
    List (List.map exprs ~f:(peval_expr scope))

  | Record fields ->
    Record (List.map fields ~f:(fun (name, expr) -> (name, peval_expr scope expr)))

  | Select (Record fields, field) ->
    begin match List.Assoc.find fields ~equal:String.equal field with
      | Some expr -> peval_expr scope expr
      | None -> Select (Record fields, field)
    end

  | Select (expr, field) ->
    Select (peval_expr scope expr, field)

  | Index (List exprs, Integer n) as expr ->
    begin match List.nth exprs (Int.of_string n) with
      | Some expr -> peval_expr scope expr
      | None -> expr
    end

  | Index (List exprs, index) ->
    Index (List exprs, peval_expr scope index)

  | Index (Record fields, String field) as expr ->
    begin match List.Assoc.find fields ~equal:String.equal field with
      | Some expr -> peval_expr scope expr
      | None -> expr
    end

  | Index (Record fields, index) ->
    Index (Record fields, peval_expr scope index)

  | Index (expr, index) ->
    Index (peval_expr scope expr, index)

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
    begin match left, right with
      | Integer l, Integer r -> fn (module Eval.Integer) l r
      | Float l, Float r -> fn (module Eval.Float) l r
      | String l, String r -> fn (module Eval.String) l r
      | left, right -> Comp (peval_expr scope left, cmp, peval_expr scope right)
    end

  | And (Bool l, Bool r) -> Bool (l && r)
  | And (left, right) -> And (peval_expr scope left, peval_expr scope right)

  | Or (Bool l, Bool r) -> Bool (l || r)
  | Or (left, right) -> Or (peval_expr scope left, peval_expr scope right)

  | Ternary (cond, left, right) ->
    begin match peval_expr scope cond with
      | Bool true -> left
      | Bool false -> right
      | cond -> Ternary (cond, left, right)
    end

and peval_fully_expr scope expr =
  let prev = ref expr in
  let out = ref (peval_expr scope expr) in
  while Ast.compare_expr !prev !out <> 0 do
    prev := !out;
    out := peval_expr scope !out;
  done;
  !out

and peval_statements scope known statements =
  let scope, statements = List.fold statements
      ~init:(scope, [])
      ~f:(fun (scope, acc) -> function
          | (Some name, expr) ->
            let expr = peval_fully_expr scope expr in
            let scope = Map.set scope ~key:name ~data:expr in
            if fully_known (Set.union known @@ scope_keys scope) expr
            then scope, acc
            else scope, (Some name, expr) :: acc
          | (None, expr) ->
            let expr = peval_fully_expr scope expr in
            scope, (None, expr) :: acc)
  in
  scope, List.rev statements

and peval_program program =
  snd @@ peval_statements (Map.empty (module String)) (Set.empty (module String)) program
