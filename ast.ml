open Base

type expr =
  | Ident of string
  | Integer of string
  | Float of string
  | Duration of string
  | Time of string
  | Regex of string
  | Char of char
  | Plus of expr * expr
  | Minus of expr * expr
  | Times of expr * expr
  | Div of expr * expr
  | Uminus of expr
  | String of string
  | Func of (string * default option) list * expr
  | Call of expr * (string * expr) list
  | Pipe of expr * expr
  | List of expr list
  | Record of (string * expr) list
  | Select of expr * string
  | Index of expr * expr
  | Assign of string * expr
  | Comp of expr * string * expr
  | And of expr * expr
  | Or of expr * expr
  | Return of expr

and default =
  | DExpr of expr
  | DPipe
[@@deriving sexp]
