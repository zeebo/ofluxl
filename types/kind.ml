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

  let invalid kind =
    match unwrap kind with
    | Cls _ -> false
    | Record { fields; lower; _ } ->
      Set.exists lower ~f:(fun field ->
          match Map.find fields field with
          | None -> false
          | Some typ ->
            match Type.unwrap typ with
            | Invalid -> true
            | _ -> false)
end

module Fixed = Make(Tc.Identity)
include Make(Tc.Ref)
