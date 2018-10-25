open Std
open Types

type t = typ Map.M(String).t
[@@deriving sexp]

let print ctx = print_endline @@ Sexp.to_string_hum @@ sexp_of_t ctx

let empty: t = Map.empty (module String)

let singleton name typ: t = Map.singleton (module String) name typ

let rec subst_typ name typ = function
  | Basic _ as typ -> typ
  | Invalid as typ -> typ
  | List typ' -> List (subst_typ name typ typ')
  | Variable name' as typ' -> if String.equal name' name then typ else typ'
  | Record name' as typ' -> if String.equal name' name then typ else typ'
  | Func { args; table; required; ret } ->
    Func { args = Map.map args ~f:(subst_typ name typ)
         ; table
         ; required
         ; ret = subst_typ name typ ret
         }

let subst_kind name typ kind =
  match kind with
  | KCls _ as kind -> kind
  | KRecord { fields; upper; lower } ->
    KRecord { fields = Map.map fields ~f:(subst_typ name typ)
            ; upper
            ; lower
            }

let apply_typ (subst: t) typ =
  Map.fold subst ~init:typ ~f:(fun ~key ~data typ ->
      subst_typ key data typ)

let apply_kind (subst: t) kind =
  Map.fold subst ~init:kind ~f:(fun ~key ~data kind ->
      subst_kind key data kind)

let merge (substl: t) (substr: t): t =
  let subst = Map.map substr ~f:(apply_typ substl) in
  Map.merge substl subst ~f:(fun ~key:_ -> function
      | `Both (_, right) -> Some right
      | `Right right -> Some right
      | `Left left -> Some left
    )
