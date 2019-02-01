open Ofluxl_std
open Ofluxl_syntax

type t =
  | InvalidExpr of Ast.expr
  | InvalidGroup of string list * string list
  | InvalidTables of string list * string list
[@@deriving sexp_of]

exception Futhark of t [@@deriving sexp_of]

let throw err = raise @@ Futhark err
