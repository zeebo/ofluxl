open Ofluxl_std

open Tvar

type ctm =
  | LetVal of Var.t * cval * ctm
  | LetPi of Var.t * int * Var.t * ctm
  | LetCont of Cont.t * Var.t * ctm * ctm
  | ContApp of Cont.t * Var.t
  | App of Var.t * Cont.t * Var.t
  | Case of Var.t * (Tag.t * Cont.t) list
  | Halt of Var.t

and cval =
  | Prim of Prim.t
  | Tuple of Var.t list
  | Tag of Tag.t * Var.t
  | Lam of Cont.t * Var.t * ctm

let rec ctm_to_string = function
  | LetVal (x, v, k) -> sprintf "letval %s = (%s) in (%s)" (Var.to_string x) (cval_to_string v) (ctm_to_string k)
  | LetPi (x, n, z, k) -> sprintf "let %s = pi_%d %s in (%s)" (Var.to_string x) n (Var.to_string z) (ctm_to_string k)
  | LetCont (k, x, k', l) -> sprintf "letcont %s %s = (%s) in (%s)" (Cont.to_string k) (Var.to_string x) (ctm_to_string k') (ctm_to_string l)
  | ContApp (k, x) -> sprintf "%s %s" (Cont.to_string k) (Var.to_string x)
  | App (f, k, x) -> sprintf "%s %s %s" (Var.to_string f) (Cont.to_string k) (Var.to_string x)
  | Case (x, ks) -> sprintf "case %s of %s" (Var.to_string x) (
      List.map ks ~f:(fun (t, k) -> sprintf "| %s -> %s" (Tag.to_string t) (Cont.to_string k))
      |> String.concat ~sep:" ")
  | Halt x -> sprintf "halt %s" (Var.to_string x)

and cval_to_string = function
  | Prim p -> Prim.to_string p
  | Tuple xs -> sprintf "(%s)" (List.map xs ~f:(fun x -> sprintf "%s" (Var.to_string x)) |> String.concat ~sep:", ")
  | Tag (t, x) -> sprintf "%s %s" (Tag.to_string t) (Var.to_string x)
  | Lam (k, x, k') -> sprintf "λ %s %s . (%s)" (Cont.to_string k) (Var.to_string x) (ctm_to_string k')

let halt z = Halt z