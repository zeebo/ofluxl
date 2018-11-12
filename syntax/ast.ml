open Ofluxl_std

(* TODO: have some validation methods that make sure
 * some properties are true, like pipes only happen
 * on calls, or theres only one DPipe default per
 * function definition, or the names are all unique
 * or whatever.
*)

type expr =
  | Ident of string
  | Integer of string
  | Float of string
  | Duration of string
  | Time of string
  | Regex of string
  | Char of char
  | String of string
  | Plus of expr * expr
  | Minus of expr * expr
  | Times of expr * expr
  | Div of expr * expr
  | Uminus of expr
  | Func of (string * default option) list * statement list
  | Call of expr * (string * expr) list
  | Pipe of expr * expr
  | List of expr list
  | Record of (string * expr) list
  | Select of expr * string
  | Index of expr * expr
  | Comp of expr * string * expr
  | And of expr * expr
  | Or of expr * expr
  | Return of expr

and default =
  | DExpr of expr
  | DPipe

and statement =
  | Assign of string * expr
  | Expr of expr
[@@deriving sexp_of]

type program = statement list
[@@deriving sexp_of]

exception Invalid of expr
