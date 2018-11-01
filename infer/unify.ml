open Err

open Ofluxl_std
open Ofluxl_types

let unify = (object (self)
  method typs (ctx: Context.ut) left right: Type.typ =
    printf "unify %s with %s\n"
      (sexp_sprint Type.sexp_of_t left)
      (sexp_sprint Type.sexp_of_t right);

    match Type.unwrap left, Type.unwrap right with
    | Variable _, typr -> typr
    | List typl, List typr -> List (ctx#unify_typs self typl typr)
    | Func {args = argsl; table = tablel; required = requiredl; ret = retl },
      Func {args = argsr; table = tabler; required = requiredr; ret = retr } ->

      if not @@ Bool.equal tablel tabler ||
         not @@ Set.is_subset requiredl ~of_:requiredr
      then
        raise @@ Infer (MismatchedTypes (Fix.typ left, Fix.typ right))
      else
        Func
          { table = tabler
          ; required = requiredr
          ; ret = ctx#unify_typs self retl retr
          ; args = Map.merge argsl argsr ~f:(fun ~key -> function
                | `Left typ ->
                  if Set.mem requiredl key
                  then raise @@ Infer (MismatchedTypes (Fix.typ left, Fix.typ right))
                  else Some typ

                | `Right _ ->
                  raise @@ Infer (MismatchedTypes (Fix.typ left, Fix.typ right))

                | `Both (typl, typr) ->
                  Some (ctx#unify_typs self typl typr))
          }

    | typl, typr ->
      if not @@ Type.equal_typ typl typr
      then raise @@ Infer (MismatchedTypes (Fix.typ left, Fix.typ right))
      else typr

  method kinds ctx left right: Kind.kind =
    printf "unify %s with %s\n"
      (sexp_sprint Kind.sexp_of_t left)
      (sexp_sprint Kind.sexp_of_t right);

    match Kind.unwrap left, Kind.unwrap right with
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
          | `Both (left, right) -> Some (ctx#unify_typs self ~strict:false left right)
        )
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

    | _ -> raise @@ Infer (MismatchedKinds (Fix.kind left, Fix.kind right))
end :> Context.unifier)