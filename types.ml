open Std

module Tvar = struct include String end

type basic =
  | Integer
  | Float
  | Duration
  | Time
  | Regex
  | Char
  | String
  | Bool
  | Table

and typ =
  | Variable of Tvar.t
  | Basic of basic
  | List of typ
  | Func of func
  | Invalid

and func =
  { args: typ Map.M(String).t
  ; table: bool
  ; required: Set.M(String).t
  ; ret: typ
  }

and kind =
  | KRecord of record
  | KCls of cls

(* it is important for unification that these classes
 * have a strict containment heirarchy *)
and cls =
  | Cmp (* most basics, strings, numbers *)
  | Add (* strings, numbers *)
  | Num (* numbers *)

and record =
  { fields: typ Map.M(String).t
  ; upper: Set.M(String).t option
  ; lower: Set.M(String).t
  }

and scheme = typ * Set.M(String).t

[@@deriving sexp, compare]

let print fn value = print_endline @@ Sexp.to_string_hum @@ fn value
let print_typ = print sexp_of_typ
let print_scheme = print sexp_of_scheme

let rec occurs name = function
  | Variable tvar -> String.equal tvar name
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
