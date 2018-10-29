open Std

type t =
  | Variable of Tvar.t
  | Basic of basic
  | List of t
  | Func of func
  | Invalid

and basic =
  | Integer
  | Float
  | Duration
  | Time
  | Regex
  | Char
  | String
  | Bool
  | Table

and func =
  { args: t Map.M(String).t
  ; table: bool
  ; required: Set.M(String).t
  ; ret: t
  }
[@@deriving sexp_of, compare]

let rec occurs name = function
  | Variable tvar -> Tvar.equal tvar name
  | Basic _ -> false
  | List typ -> occurs name typ
  | Func { args; _ } -> Map.exists args ~f:(occurs name)
  | Invalid -> false
