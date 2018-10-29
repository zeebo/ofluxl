open Ofluxl_std

module Make(Tc: Tc.S) = struct
  type t = typ Tc.t

  and typ =
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

  let wrap: typ -> t = Tc.wrap
  let unwrap: t -> typ = Tc.unwrap

  let rec occurs name typ =
    match unwrap typ with
    | Variable tvar -> Tvar.equal tvar name
    | Basic _ -> false
    | List typ -> occurs name typ
    | Func { args; _ } -> Map.exists args ~f:(occurs name)
    | Invalid -> false
end

module Fixed = Make(Tc.Identity)
include Make(Tc.Ref)
