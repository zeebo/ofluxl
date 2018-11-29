open Ofluxl_std
open Ofluxl_types

type t = Scheme.t Map.M(String).t [@@deriving sexp_of]

let default: t =
  let mk name typ = (name, Scheme.empty typ) in

  Map.of_alist_exn (module String)
    [ mk "true" @@ Basic Bool
    ; mk "false" @@ Basic Bool
    ]

let merge (env: t) args: t =
  Map.merge env args ~f:(fun ~key:_ -> function
      | `Both (_, right) -> Some right
      | `Left left -> Some left
      | `Right right -> Some right
    )

let set (env: t) name scheme: t =
  Map.set env ~key:name ~data:scheme

let find (env: t) name =
  Map.find env name

let substitute mapping env: t =
  Map.map env ~f:(Scheme.substitute mapping)

let ftv env =
  Map.data env
  |> List.map ~f:Scheme.ftv
  |> Set.union_list (module Tvar)
