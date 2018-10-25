open Std
open Types

type t =
  { mutable typ_constraints : (typ * typ) list
  ; mutable kind_constraints : (string * kind) list
  ; mutable num: int
  }
[@@deriving sexp]

let print ctx = print_endline @@ Sexp.to_string_hum @@ sexp_of_t ctx

let create (): t =
  { typ_constraints = []
  ; kind_constraints = []
  ; num = 0
  }

let typ_constraints { typ_constraints; _ } = typ_constraints
let kind_constraints { kind_constraints; _ } = kind_constraints

let add_typ_constraint ctx (left, right) =
  ctx.typ_constraints <- (left, right) :: ctx.typ_constraints

let add_kind_constraint ctx (name, kind) =
  ctx.kind_constraints <- (name, kind) :: ctx.kind_constraints

let fresh_type_name ctx =
  ctx.num <- ctx.num + 1;
  Printf.sprintf "a%d" ctx.num
