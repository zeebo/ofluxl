open Std

module Tvar: sig
  type t [@@deriving sexp_of, compare]
  include Comparator.S with type t := t
  val equal: t -> t -> bool
  val of_string: string -> t
  val to_string: t -> string
end = struct
  include String
  let of_string t = t
  let to_string t = t
end

type typ =
  | Variable of Tvar.t
  | Basic of basic
  | List of typ
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
  { args: typ Map.M(String).t
  ; table: bool
  ; required: Set.M(String).t
  ; ret: typ
  }
[@@deriving sexp_of, compare]

type kind =
  | KRecord of record
  | KCls of cls

and cls =
  | Cmp
  | Add
  | Num

and record =
  { fields: typ Map.M(String).t
  ; upper: Set.M(String).t option
  ; lower: Set.M(String).t
  }
[@@deriving sexp_of]

type scheme = typ * Set.M(Tvar).t
[@@deriving sexp_of]

let rec occurs name = function
  | Variable tvar -> Tvar.equal tvar name
  | Basic _ -> false
  | List typ -> occurs name typ
  | Func { args; _ } -> Map.exists args ~f:(occurs name)
  | Invalid -> false

let invalid_kind = function
  | KCls _ -> false
  | KRecord { fields; lower; _ } ->
    Set.exists lower ~f:(fun field ->
        match Map.find fields field with
        | Some Invalid -> true
        | _ -> false)

