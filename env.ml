open Std
open Types

type t = scheme Map.M(String).t [@@deriving sexp_of]

let print env = print_endline @@ Sexp.to_string_hum @@ sexp_of_t env

let empty: t =
  let mk name typ = (name, (typ, Set.empty (module String))) in
  Map.of_alist_exn (module String)
    [ mk "true" @@ Basic Bool
    ; mk "false" @@ Basic Bool
    ; mk "gen" @@ Func
        { args = Map.empty (module String)
        ; table = false
        ; required = Set.empty (module String)
        ; ret = Basic Table
        }
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
