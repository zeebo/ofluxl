open Ofluxl_std

open Tvar

type ml =
  | Var of Var.t
  | App of ml * ml
  | Lam of Var.t * ml
  | Tuple of ml list
  | Pi of int * ml
  | Void
  | Tag of Tag.t * ml
  | Let of Var.t * ml * ml
  | Case of ml * (Tag.t * Var.t * ml) list
