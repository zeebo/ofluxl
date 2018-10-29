open Ofluxl_std
open Ofluxl_types

type t = Scheme.t Map.M(String).t [@@deriving sexp_of]

let empty: t =
  let mk name typ = (name, Scheme.empty @@ Type.wrap typ) in

  Map.of_alist_exn (module String)
    [ mk "true" @@ Type.Basic Bool
    ; mk "false" @@ Type.Basic Bool
    ]

let insert (env: t) args: t =
  Map.merge env args ~f:(fun ~key:_ -> function
      | `Both (_, right) -> Some right
      | `Left left -> Some left
      | `Right right -> Some right
    )

let set (env: t) name scheme: t =
  Map.set env ~key:name ~data:scheme

let find (env: t) name =
  Map.find env name
