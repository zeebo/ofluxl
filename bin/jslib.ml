open Ofluxl
open Std

let rec build ?(depth=0) cb =
  let t = object (self)
    val mutable lines = []
    val depth = depth

    method add line =
      let line = String.make depth '\t' ^ line in
      lines <- line :: lines

    method finish =
      String.concat ~sep:"\n" @@ List.rev lines

    method scope cb =
      self#add @@ build ~depth:(depth + 1) cb
  end in

  cb t;
  t#finish

let rec expr_to_string = function
  | Syntax.Ast.Ident name -> name
  | Integer i -> i
  | Float f -> f
  | Duration d -> d
  | Time t -> t
  | Regex r -> r
  | Char c -> sprintf "'%c'" c
  | Bool b -> sprintf "%b" b
  | String s -> sprintf "\"%s\"" s (* TODO(jeff): quoting is wrong *)
  | Plus (l, r) -> sprintf "((%s) + (%s))" (expr_to_string l) (expr_to_string r)
  | Minus (l, r) -> sprintf "((%s) - (%s))" (expr_to_string l) (expr_to_string r)
  | Times (l, r) -> sprintf "((%s) * (%s))" (expr_to_string l) (expr_to_string r)
  | Div (l, r) -> sprintf "((%s) / (%s))" (expr_to_string l) (expr_to_string r)
  | Uminus e -> sprintf "(-(%s))" (expr_to_string e)
  | Call (expr, args) -> sprintf "((%s)(%s))" (expr_to_string expr) (make_args args)
  | Pipe (left, right) -> sprintf "((%s) |> (%s))" (expr_to_string left) (expr_to_string right)
  | List exprs -> sprintf "[%s]" (String.concat ~sep:", " (List.map exprs ~f:expr_to_string))
  | Record fields -> sprintf "{%s}" (String.concat ~sep:", " (List.map fields ~f:(fun (name, e) -> sprintf "%s: %s" name (expr_to_string e))))
  | Select (expr, field) -> sprintf "(%s).%s" (expr_to_string expr) field
  | Index (expr, index) -> sprintf "((%s)[%s])" (expr_to_string expr) (expr_to_string index)
  | Comp (left, cmp, right) -> sprintf "((%s) %s (%s))" (expr_to_string left) cmp (expr_to_string right)
  | And (left, right) -> sprintf "((%s) && (%s))" (expr_to_string left) (expr_to_string right)
  | Or (left, right) -> sprintf "((%s) || (%s))" (expr_to_string left) (expr_to_string right)
  | Ternary (cond, left, right) -> sprintf "((%s) ? (%s) : (%s))" (expr_to_string cond) (expr_to_string left) (expr_to_string right)
  | Func (params, body, ret) ->
    build (fun t ->
        t#add @@ sprintf "(%s) => {" (make_params params);
        t#scope (fun t ->
            List.iter body ~f:(fun statement -> t#add @@ statement_to_string statement ^ ";");
            t#add @@ sprintf "return %s" (expr_to_string ret));
        t#add "}")

and statement_to_string = function
  | Syntax.Ast.Assign (name, expr) -> sprintf "%s = %s;" name (expr_to_string expr)
  | Expr expr -> sprintf "%s;" (expr_to_string expr)

and program_to_string program =
  build (fun t -> List.iter program ~f:(fun statement -> t#add @@ statement_to_string statement))

and make_params params =
  List.map params ~f:(function
      | name, Some (Syntax.Ast.DExpr expr) -> sprintf "%s=%s" name (expr_to_string expr)
      | name, Some Syntax.Ast.DPipe -> sprintf "%s=<-" name
      | name, None -> name)
  |> String.concat ~sep:", "

and make_args args =
  List.map args ~f:(fun (name, expr) -> sprintf "%s: %s" name (expr_to_string expr))
  |> String.concat ~sep:", "

let peval str =
  match Syntax.Parse.str str with
  | Ok program ->
    program
    |> Partial.peval_program
    |> program_to_string
  | Error err ->
    Syntax.Err.to_string err

let () =
  Js.export_all (object%js
    method peval str = str |> Js.to_string |> peval |> Js.string
  end)