open Ofluxl_std
open Ofluxl_types

type t = Type.t * Set.M(Tvar).t [@@deriving sexp_of]

let empty typ: t = (typ, Set.empty (module Tvar))

let ftv (typ, ftv) = Set.union (Type.ftv typ) ftv

let substitute mapping (typ, ftv): t =
  let ftv = Set.filter_map (module Tvar) ftv ~f:(fun name ->
      match Map.find mapping name with
      | None -> Some name
      | Some typ -> match typ with
        | Type.Variable name' -> Some name'
        | _ -> None) in

  (Type.substitute mapping typ, ftv)
