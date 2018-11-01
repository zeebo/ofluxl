open Ofluxl_std

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

let equal left right =
  compare left right = 0

let rec occurs name typ =
  match typ with
  | Variable name' -> Tvar.equal name name'
  | Basic _ -> false
  | List typ -> occurs name typ
  | Func { args; _ } -> Map.exists args ~f:(occurs name)
  | Invalid -> false

let rec substitute mapping typ =
  match typ with
  | Invalid -> typ
  | Basic _ -> typ
  | List typ' -> List (substitute mapping typ')
  | Func func ->
    let args = Map.map func.args ~f:(substitute mapping) in
    let ret = substitute mapping func.ret in
    Func { func with args; ret }
  | Variable name ->
    match Hashtbl.find mapping name with
    | None -> typ
    | Some typ -> typ

let rec ftv typ =
  match typ with
  | Variable name -> Set.singleton (module Tvar) name
  | List typ -> ftv typ
  | Func func ->
    let args = Map.data func.args |> List.map ~f:ftv in
    let ret = ftv func.ret in
    Set.union ret @@ Set.union_list (module Tvar) args
  | _ -> Set.empty (module Tvar)
