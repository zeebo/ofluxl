open Std
open Types

type t =
  { mutable typ_constraints : (typ * typ) list
  ; mutable kind_constraints : kind list Map.M(String).t
  ; mutable num: int
  }
[@@deriving sexp_of]

let create (): t =
  { typ_constraints = []
  ; kind_constraints = Map.empty (module String)
  ; num = 0
  }

let typ_constraints { typ_constraints; _ } = typ_constraints
let kind_constraints { kind_constraints; _ } = kind_constraints

let add_typ_constraint ctx (left, right) =
  ctx.typ_constraints <- (left, right) :: ctx.typ_constraints

let add_kind_constraint ctx (name, kind) =
  ctx.kind_constraints <-
    Map.update ctx.kind_constraints name ~f:(function
        | Some kinds -> kind :: kinds
        | None -> [kind])

let fresh_type_name ctx =
  ctx.num <- ctx.num + 1;
  Printf.sprintf "a%d" ctx.num

let rec ftv ctx = function
  | Variable name ->
    let ftv = match Map.find ctx.kind_constraints name with
      | Some kinds ->
        List.fold kinds
          ~init:(Set.empty (module String))
          ~f:(fun ftv kind -> Set.union ftv (ftv_kind ctx kind))
      | None -> Set.empty (module String)
    in Set.add ftv name

  | Basic _ | Invalid -> Set.empty (module String)
  | List typ -> ftv ctx typ
  | Func { args; ret; _ } ->
    args
    |> Map.data
    |> List.map ~f:(ftv ctx)
    |> List.fold ~init:(Set.empty (module String)) ~f:Set.union
    |> Set.union (ftv ctx ret)

and ftv_kind ctx = function
  | KRecord { fields; _ } ->
    fields
    |> Map.data
    |> List.map ~f:(ftv ctx)
    |> List.fold ~init:(Set.empty (module String)) ~f:Set.union

  | _ -> Set.empty (module String)

let inst ctx (typ, ftv) =
  (* create a substitution for the free type variables to be fresh *)
  let subst = Set.fold ftv ~init:Subst.empty ~f:(fun subst name ->
      let name' = fresh_type_name ctx in
      let subst' = Subst.singleton name (Variable name') in
      Subst.merge subst subst')
  in

  (* copy in all of the kind constraints for the new type variables *)
  Set.iter ftv ~f:(fun name ->
      match Map.find ctx.kind_constraints name with
      | Some kinds ->
        List.iter kinds ~f:(fun kind ->
            let name' = Subst.apply_name subst name in
            let kind' = Subst.apply_kind subst kind in
            add_kind_constraint ctx (name', kind'))
      | None -> ());

  (* copy in all of the type constraints for the new type variables *)
  List.iter ctx.typ_constraints ~f:(fun (left, right) ->
      let left' = Subst.apply_typ subst left in
      let right' = Subst.apply_typ subst right in
      match (compare_typ left left', compare_typ right right') with
      | (0, 0) -> ()
      | _ -> add_typ_constraint ctx (left', right'));

  (* apply the substitution to the type *)
  Subst.apply_typ subst typ
