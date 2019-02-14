open Ofluxl_std

open Tvar

type ml =
  | Var of Var.t
  | App of ml * ml
  | Lam of Var.t * ml
  | Tuple of ml list
  | Pi of int * ml
  | Prim of Prim.t
  | Tag of Tag.t * ml
  | Let of Var.t * ml * ml
  | Case of ml * (Tag.t * Var.t * ml) list

let rec to_string = function
  | Var x -> Var.to_string x
  | App (func, arg) -> sprintf "%s %s" (to_string func) (to_string arg)
  | Lam (x, body) -> sprintf "λ %s . %s" (Var.to_string x) (to_string body)
  | Tuple exprs -> sprintf "[%s]" (List.map exprs ~f:to_string |> String.concat ~sep:"; ")
  | Pi (n, expr) -> sprintf "%s[%d]" (to_string expr) n
  | Prim p -> Prim.to_string p
  | Tag (t, expr) -> sprintf "%s => %s" (Tag.to_string t) (to_string expr)
  | Let (x, expr, body) -> sprintf "let %s = %s in %s end" (Var.to_string x) (to_string expr) (to_string body)
  | Case _ -> "case of ... end"