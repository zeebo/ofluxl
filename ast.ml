open Base

type expr =
  | Ident of string
  | Number of string
  | Plus of expr * expr
  | Minus of expr * expr
  | Times of expr * expr
  | Div of expr * expr
  | Uminus of expr
  | String of string
  | Func of string list * expr
  | Call of expr * (string * expr) list
  | Pipe of expr * expr
  | List of expr list
  | Record of (string * expr) list
  | Select of expr * string
  | Index of expr * expr
  | Assign of string * expr

let rec string_of_expr = function
  | Ident str -> Printf.sprintf "Ident(%s)" str
  | Number str -> Printf.sprintf "Number(%s)" str
  | Plus (e1, e2) -> Printf.sprintf "Plus(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | Minus (e1, e2) -> Printf.sprintf "Minus(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | Times (e1, e2) -> Printf.sprintf "Times(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | Div (e1, e2) -> Printf.sprintf "Div(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | Uminus e -> Printf.sprintf "Uminus(%s)" (string_of_expr e)
  | String str -> Printf.sprintf "String(%s)" str
  | Func (params, body) -> Printf.sprintf "Func((%s), %s)" (String.concat ~sep:", " params) (string_of_expr body)
  | Call (expr, args) -> Printf.sprintf "Call(%s, (%s))" (string_of_expr expr) (join args |> String.concat ~sep:", ")
  | Pipe (e1, e2) -> Printf.sprintf "Pipe(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | List exprs -> Printf.sprintf "List(%s)" (List.map exprs ~f:string_of_expr |> String.concat ~sep:", ")
  | Record values -> Printf.sprintf "Record(%s)" (join values |> String.concat ~sep:", ")
  | Select (expr, field) -> Printf.sprintf "Select(%s, %s)" (string_of_expr expr) field
  | Index (expr, index) -> Printf.sprintf "Index(%s, %s)" (string_of_expr expr) (string_of_expr index)
  | Assign (name, expr) -> Printf.sprintf "Assign(%s, %s)" name (string_of_expr expr)

and join ?delim:(delim=":") entries =
  List.map entries ~f:(fun (name, expr) -> Printf.sprintf "%s%s%s" name delim (string_of_expr expr))
