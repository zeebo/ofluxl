open Std

type tvar = string

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

and typ =
  | Variable of tvar
  | Basic of basic
  | List of typ
  | Func of func
  | Record of tvar
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

[@@deriving sexp]

exception Infinite of (string * typ)

let rec occurs name = function
  | Variable tvar -> String.equal tvar name
  | Basic _ -> false
  | List typ -> occurs name typ
  | Func { args; _ } -> Map.exists args ~f:(occurs name)
  | Invalid -> false
  | Record tvar -> String.equal tvar name

let rec invalid = function
  | List typ -> invalid typ
  | Func { args; _ }  -> Map.exists args ~f:invalid
  | Invalid -> true
  | _ -> false
