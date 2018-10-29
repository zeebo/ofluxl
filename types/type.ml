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

(* convert maps a type from one type constructor to another *)
module Convert (FromTc: Tc.S) (ToTc: Tc.S) = struct
  let convert =
    let module From = Make(FromTc) in
    let module To = Make(ToTc) in

    let rec convert from = To.wrap @@
      match From.unwrap from with
      | From.Invalid -> To.Invalid
      | List typ -> List (convert typ)
      | Func { args; table; required; ret } ->
        Func { args = Map.map args ~f:convert ; table ; required; ret = convert ret }
      | Variable tvar -> Variable tvar
      | Basic Integer -> Basic Integer
      | Basic Float -> Basic Float
      | Basic Duration -> Basic Duration
      | Basic Time -> Basic Time
      | Basic Regex -> Basic Regex
      | Basic Char -> Basic Char
      | Basic String -> Basic String
      | Basic Bool -> Basic Bool
      | Basic Table -> Basic Table

    in convert
end

module RefTc = struct
  type 'a t = 'a ref [@@deriving sexp, compare]
  let wrap a = ref a
  let unwrap t = !t
end

module Ref = Make(RefTc)

module FixedTc = struct
  type 'a t = 'a [@@deriving sexp, compare]
  let wrap a = a
  let unwrap t = t
end

module Fixed = Make(FixedTc)

include Ref

let fix =
  let module Fix = Convert(RefTc)(FixedTc) in
  Fix.convert
