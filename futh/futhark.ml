open Err

open Ofluxl_std
open Ofluxl_syntax

open Table

let root = {Table.group = []; columns = ["a"; "b"; "c"; "d"]; index = Table.counter#next}

let ctx = object
  val mutable table = root
  val mutable tables = [root]
  val mutable genned = []

  method table = table

  method set_table (tab: table) =
    if List.fold (List.map tables ~f:(fun t -> t.index = tab.index)) 
        ~init:false 
        ~f:(fun a b -> a || b)
    then ()
    else tables <- tab :: tables;

    table <- tab

  method tables = tables

  method add_gen str = genned <- str :: genned

  method genned = genned
end

let generate_agg table column op =
  let typ = (type_name_of table) in
  let t = make_tracker () in
  let name = sprintf "agg_%s_%d" typ counter#next in
  t#add @@ sprintf "let %s (rs: []%s): %s = " name typ typ;
  t#add @@ sprintf "\tlet combined = map (\\r -> r.%s) rs |> %s in" column op;
  t#add @@ sprintf "\trs[0] with %s = combined" column;
  name, t#finish

let generate_sum table column = 
  let eq_name, code = generate_equal table in
  ctx#add_gen code;
  let agg_name, code = generate_agg table column "f64.sum" in
  ctx#add_gen code;
  sprintf "stdlib_agg %s %s" agg_name eq_name

let args_helper = function
  | [] -> ["()"]
  | x -> x 

let rec string_of_statement s =
  begin match s with
    | Ast.Assign (n, e) -> sprintf "let %s = %s in" n (string_of_expr e)
    | Expr e -> sprintf "let _ = %s in" (string_of_expr e)
  end ^ "\n"

and string_of_expr e =
  match e with
  | Ast.Ident i -> i
  | Integer i -> i
  | Float f -> f
  | String s -> s
  | Plus (l, r) -> sprintf "((%s) + (%s))" (string_of_expr l) (string_of_expr r)
  | Minus (l, r) -> sprintf "((%s) - (%s))" (string_of_expr l) (string_of_expr r)
  | Times (l, r) -> sprintf "((%s) * (%s))" (string_of_expr l) (string_of_expr r)
  | Div (l, r) -> sprintf "((%s) / (%s))" (string_of_expr l) (string_of_expr r)
  | Uminus e -> sprintf "(-(%s))" (string_of_expr e)
  | Pipe (l, r) -> 
    let ls = (string_of_expr l) in
    let rs = (string_of_expr r) in
    sprintf "((%s) |> (%s))" ls rs
  | List es -> sprintf "[%s]" (String.concat ~sep:", " (List.map es ~f:string_of_expr))
  | Record re -> sprintf "{%s}" (String.concat ~sep:", " (List.map re ~f:(fun (name, e) -> sprintf "%s = %s" name (string_of_expr e))))
  | Select (e, f) -> sprintf "(%s).%s" (string_of_expr e) f
  | Index (e, i) -> sprintf "((%s)[%s])" (string_of_expr e) (string_of_expr i)
  | Comp (l, c, r) -> sprintf "((%s) %s (%s))" (string_of_expr l) c (string_of_expr r)
  | And (l, r) -> sprintf "((%s) && (%s))" (string_of_expr l) (string_of_expr r)
  | Or (l, r) -> sprintf "((%s) || (%s))" (string_of_expr l) (string_of_expr r)
  | Return e -> string_of_expr e
  | Func (args, body, ret) ->
    let args =
      args 
      |> List.map ~f:(fun (name, def) -> match def with
          | Some Ast.DPipe -> name
          | Some _ -> throw @@ InvalidExpr e
          | None -> name)
      |> List.sort ~compare:String.compare 
      |> args_helper
      |> String.concat ~sep:" "
    and body = 
      body
      |> List.map ~f:string_of_statement
      |> String.concat ~sep:""
    and ret = string_of_expr ret
    in
    sprintf "(\\%s -> %s %s)" args body ret

  | Call (Ident "drop", ["columns", Ast.List columns]) ->
    let columns = List.map columns ~f:(function
        | Ast.String column -> column
        | _ -> throw @@ InvalidExpr e)
    in
    let old_table = ctx#table in
    let table = with_drop old_table columns in
    ctx#set_table table;
    let name, code = generate_table_change old_table table in
    ctx#add_gen code;
    name

  | Call (Ident "group", ["columns", Ast.List columns]) ->
    let columns = List.map columns ~f:(function
        | Ast.String column -> column
        | _ -> throw @@ InvalidExpr e)
    in
    let old_table = ctx#table in
    let table = with_group old_table columns in
    ctx#set_table table;
    let name, code = generate_table_change old_table table in
    ctx#add_gen code;
    name

  | Call (Ident "sum", []) ->
    generate_sum ctx#table "_value"

  | Call (Ident "sum", ["columns", Ast.List [String column]]) ->
    generate_sum ctx#table column

  | Call (e, args) ->
    let args = 
      args 
      |> List.sort ~compare:(fun (n1, _) (n2, _) -> String.compare n1 n2)
      |> List.map ~f:(fun (_, e) -> string_of_expr e)
      |> args_helper
      |> String.concat ~sep:" "
    in
    sprintf "(%s) %s" (string_of_expr e) args

  | _ -> throw @@ InvalidExpr e

let () =
  match Parse.stdin () with
  | Ok program ->
    print_endline {|
import "lib/github.com/diku-dk/sorts/merge_sort"
import "stdlib"
|};

    let code = List.map program ~f:(fun s -> string_of_statement s) in
    List.iter ctx#tables ~f:(fun table -> print_endline (type_def_of table));
    List.iter ctx#genned ~f:print_endline;

    print_endline {|
let main a b c d =
  let table = map4 (\a b c d -> {a, b, c, d}) a b c d in
|};

    List.iter code ~f:print_endline;

    print_endline {|
(map (\r -> r.b) output, map (\r -> r.d) output)
|};

  | Error err -> Err.print err
