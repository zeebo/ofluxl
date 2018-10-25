open Std
open Types

type t = typ Map.M(String).t
[@@deriving sexp]

let print env = print_endline @@ Sexp.to_string_hum @@ sexp_of_t env

let empty: t = Map.of_alist_exn (module String)
    [ ("true", Basic Bool)
    ; ("false", Basic Bool)
    ]

let insert (env: t) (args: t): t =
  Map.merge env args ~f:(fun ~key:_ -> function
      | `Both (_, right) -> Some right
      | `Left left -> Some left
      | `Right right -> Some right
    )

let find (env: t) name =
  Map.find env name
