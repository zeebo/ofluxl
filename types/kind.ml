open Ofluxl_std

type t =
  | Record of record
  | Cls of cls

and cls =
  | Cmp
  | Add
  | Num

and record =
  { fields: Type.t Map.M(String).t
  ; upper: Set.M(String).t option
  ; lower: Set.M(String).t
  }
[@@deriving sexp_of]

let invalid = function
  | Cls _ -> false
  | Record { fields; lower; _ } ->
    Set.exists lower ~f:(fun field ->
        match Map.find fields field with
        | Some Type.Invalid -> true
        | _ -> false)

