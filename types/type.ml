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
  let refresh t = wrap @@ unwrap t

  let equal_typ left right =
    compare_typ left right = 0

  let rec occurs name typ =
    match unwrap typ with
    | Variable name' -> Tvar.equal name name'
    | Basic _ -> false
    | List typ -> occurs name typ
    | Func { args; _ } -> Map.exists args ~f:(occurs name)
    | Invalid -> false

  let rec substitute mapping typ =
    match unwrap typ with
    | Invalid -> refresh typ
    | Basic _ -> refresh typ
    | List typ' -> wrap @@ List (substitute mapping typ')
    | Func func ->
      let args = Map.map func.args ~f:(substitute mapping) in
      let ret = substitute mapping func.ret in
      wrap @@ Func { func with args; ret }
    | Variable name ->
      match Hashtbl.find mapping name with
      | None -> refresh typ
      | Some name -> wrap @@ Variable name

  let rec ftv typ =
    match unwrap typ with
    | Variable name -> Set.singleton (module Tvar) name
    | List typ -> ftv typ
    | Func func ->
      let args = Map.data func.args |> List.map ~f:ftv in
      let ret = ftv func.ret in
      Set.union ret @@ Set.union_list (module Tvar) args
    | _ -> Set.empty (module Tvar)
end

module Fixed = Make(Tc.Identity)
include Make(Tc.Ref)
