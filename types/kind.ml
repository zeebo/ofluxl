open Ofluxl_std

module Make(Tc: Tc.S) = struct
  module Type = Type.Make(Tc)

  type t = kind Tc.t

  and kind =
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

  let wrap: kind -> t = Tc.wrap
  let unwrap: t -> kind = Tc.unwrap

  let invalid {fields; lower; _} =
    Set.exists lower ~f:begin fun field ->
      match Map.find fields field with
      | None -> false
      | Some typ ->
        match Type.unwrap typ with
        | Invalid -> true
        | _ -> false
    end

  let compatible_with_typ kind typ =
    match (unwrap kind, Type.unwrap typ) with
    | (_, Variable _) -> true
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
    let kind = unwrap kind in
    wrap @@ match kind with
    | Cls _ -> kind
    | Record record ->
      Record { record with fields = Map.map record.fields ~f:(Type.substitute mapping) }

  let ftv kind =
    match unwrap kind with
    | Cls _ -> Set.empty (module Tvar)
    | Record record ->
      let fields = Map.data record.fields |> List.map ~f:Type.ftv in
      Set.union_list (module Tvar) fields
end

module Fixed = Make(Tc.Identity)
include Make(Tc.Ref)
