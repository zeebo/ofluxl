open Err

open Ofluxl_std
open Ofluxl_types

let constrained ctx typl typr =
  let typ = ctx#fresh_variable in
  ctx#typ_constraint typl typ;
  ctx#typ_constraint typr typ;
  typ

let typs ctx (left: Type.t) (right: Type.t): Type.t =
  match left, right with
  | Variable name, typ | typ, Variable name ->
    if Type.occurs name typ
    then raise @@ Infer (Infinite (name, typ))
    else typ

  | List typl, List typr ->
    List (constrained ctx typl typr)

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
        ; ret = constrained ctx retl retr
        ; args = Map.merge argsl argsr ~f:(fun ~key -> function
              | `Left typ ->
                if Set.mem requiredl key
                then raise @@ Infer (MismatchedTypes (left, right))
                else Some typ

              | `Right _ ->
                raise @@ Infer (MismatchedTypes (left, right))

              | `Both (typl, typr) ->
                Some (constrained ctx typl typr)
            )
        }

  | typl, typr ->
    if not @@ Type.equal typl typr
    then raise @@ Infer (MismatchedTypes (left, right))
    else typr

let kinds ctx (left: Kind.t) (right: Kind.t): Kind.t =
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

  | Record { fields = fieldsl; upper = upperl; lower = lowerl; from = froml },
    Record { fields = fieldsr; upper = upperr; lower = lowerr; from = fromr } ->

    let fields = Map.merge fieldsl fieldsr ~f:(fun ~key:_ -> function
        | `Left typl -> Some typl
        | `Right typr -> Some typr
        | `Both (typl, typr) ->
          try Some (constrained ctx typl typr) with
          | Infer _ -> Some Invalid)

    and upper = match upperl, upperr with
      | None, Some upperr -> Some upperr
      | Some upperl, None -> Some upperl
      | None, None -> None
      | Some upperl, Some upperr -> Some (Set.inter upperl upperr)

    and lower = Set.union lowerl lowerr

    (* and from = Some (constrained ctx froml fromr) *)
    in

    ignore froml; ignore fromr;

    begin match upper with
      | Some upper ->
        if Set.is_subset lower ~of_:upper then ()
        else raise @@ Infer UnknownRecordAccess
      | None -> ()
    end;

    Set.iter lower ~f:(fun field ->
        match Map.find fields field with
        | Some Invalid -> raise @@ Infer (InvalidRecordAccess (field, fields))
        | _ -> ());

    Record { fields; upper; lower; from = None }

  | _ -> raise @@ Infer (MismatchedKinds (left, right))
