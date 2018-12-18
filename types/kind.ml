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

let invalid {fields; lower; _} =
  Set.exists lower ~f:(fun field ->
      match Map.find fields field with
      | Some Invalid -> true
      | _ -> false)

let compatible_with_typ kind typ =
  match kind, typ with
  | (_, Type.Variable _) -> true
  | (Cls Cmp, Basic Integer)  | (Cls Add, Basic Integer)  | (Cls Num, Basic Integer)
  | (Cls Cmp, Basic Float)    | (Cls Add, Basic Float)    | (Cls Num, Basic Float)
  | (Cls Cmp, Basic Duration) | (Cls Add, Basic Duration) | (Cls Num, Basic Duration)
  | (Cls Cmp, Basic String)   | (Cls Add, Basic String)
  | (Cls Cmp, Basic Time)
  | (Cls Cmp, Basic Regex)
  | (Cls Cmp, Basic Char)
  | (Cls Cmp, Basic Bool) -> true
  | _ -> false

let substitute mapping kind =
  match kind with
  | Cls _ -> kind
  | Record record ->
    let fields = Map.map record.fields ~f:(Type.substitute mapping) in
    Record { record with fields }

let ftv kind =
  match kind with
  | Cls _ -> Set.empty (module Tvar)
  | Record record ->
    let fields = Map.data record.fields |> List.map ~f:Type.ftv in
    Set.union_list (module Tvar) fields
