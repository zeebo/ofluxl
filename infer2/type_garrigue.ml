open Ofluxl_std

type typ =
  | Tvar of Tvar.t
  | Basic of basic
  | Fn of typ * typ

and basic =
  | Integer
  | Float
  | Duration
  | Time
  | Regex
  | Char
  | String
  | Bool
[@@deriving compare]

type labels =
  | Infinite
  | Finite of Set.M(String).t

(* encoding of records with maskable fields *)
type rel = string * typ

type con = Set.M(String).t * labels

type kind = con * rel list

type kind_env = Tvar.t Map.M(Tvar).t

type poly =
  | Mono of typ
  | Poly of Tvar.t list * kind_env * typ

type sort =
  | SBasic of basic
  | SFn
  | SKind
[@@deriving compare]

let free kind_env _poly =
  let _empty = Set.empty (module Tvar) in
  let sing = Set.singleton (module Tvar) in

  (* collect all the free type variables from the kind first *)
  kind_env
  |> Map.keys
  |> List.map ~f:sing
  |> Set.union_list (module Tvar)