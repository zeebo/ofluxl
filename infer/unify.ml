open Err

open Ofluxl_std
open Ofluxl_types

let unify = (object (self)
  method typs ctx (left: Type.t) (right: Type.t): Type.t =
    match left, right with
    | Type.Variable _, typr -> typr
    | typl, Type.Variable _ -> typl

    | List typl, List typr -> List (ctx#unify_typs self typl typr)

    | Func {args = argsl; table = tablel; required = requiredl; ret = retl },
      Func {args = argsr; table = tabler; required = requiredr; ret = retr } ->

      if not @@ Bool.equal tablel tabler ||
         not @@ Set.is_subset requiredl ~of_:requiredr
      then
        raise @@ Infer (MismatchedTypes (left, right))
      else
        Func
          { table = tabler
          ; required = requiredr
          ; ret = ctx#unify_typs self retl retr
          ; args = Map.merge argsl argsr ~f:(fun ~key -> function
                | `Left typ ->
                  if Set.mem requiredl key
                  then raise @@ Infer (MismatchedTypes (left, right))
                  else Some typ

                | `Right _ ->
                  raise @@ Infer (MismatchedTypes (left, right))

                | `Both (typl, typr) ->
                  Some (ctx#unify_typs self typl typr))
          }

    | typl, typr ->
      if not @@ Type.equal typl typr
      then raise @@ Infer (MismatchedTypes (left, right))
      else typr

  method kinds ctx (left: Kind.t) (right: Kind.t): Kind.t =
    match left, right with
    | Cls Cmp, Cls Cmp -> Cls Cmp
    | Cls Cmp, Cls Add -> Cls Add
    | Cls Cmp, Cls Num -> Cls Num
    | Cls Add, Cls Cmp -> Cls Add
    | Cls Add, Cls Add -> Cls Add
    | Cls Add, Cls Num -> Cls Num
    | Cls Num, Cls Cmp -> Cls Num
    | Cls Num, Cls Add -> Cls Num
    | Cls Num, Cls Num -> Cls Num

    | Record { fields = fieldsl; upper = upperl; lower = lowerl },
      Record { fields = fieldsr; upper = upperr; lower = lowerr } ->

      let fields = Map.merge fieldsl fieldsr ~f:(fun ~key:_ -> function
          | `Left left -> Some left
          | `Right right -> Some right
          | `Both (left, right) ->
            try Some (ctx#unify_typs self left right) with
            | Infer _ -> Some Invalid)
      in
      let upper = match upperl, upperr with
        | None, Some upperr -> Some upperr
        | Some upperl, None -> Some upperl
        | None, None -> None
        | Some upperl, Some upperr -> Some (Set.inter upperl upperr)
      in
      let lower =
        Set.union lowerl lowerr
      in

      Record { fields; upper; lower }

    | _ -> raise @@ Infer (MismatchedKinds (left, right))

end :> Context.unifier)
