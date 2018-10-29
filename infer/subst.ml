open Ofluxl_std
open Ofluxl_types

type t = Type.t Map.M(Tvar).t [@@deriving sexp_of]

let empty: t = Map.empty (module Tvar)

let singleton name typ: t = Map.singleton (module Tvar) name typ

let rec subst_typ name typ into =
  match Type.unwrap into with
  | Basic _ -> into
  | Invalid -> into
  | List typ' -> Type.wrap @@ List (subst_typ name typ typ')
  | Variable name' -> if Tvar.equal name' name then typ else into
  | Func ({ args; ret; _ } as func) -> Type.wrap @@
    Func { func with args = Map.map args ~f:(subst_typ name typ)
                   ; ret = subst_typ name typ ret }

let subst_kind name typ (kind: Kind.t) =
  match Kind.unwrap kind with
  | Kind.Cls _ -> kind
  | Record ({ fields; _ } as record) -> Kind.wrap @@
    Record { record with fields = Map.map fields ~f:(subst_typ name typ) }

let subst_scheme name typ (typ', ftv) =
  let ftv = match (Set.mem ftv name, Type.unwrap typ) with
    | (true, Type.Variable name') -> Set.add ftv name'
    | _ -> ftv
  in
  let ftv = Set.remove ftv name in
  (subst_typ name typ typ', ftv)

let apply_typ (subst: t) (typ: Type.t) =
  Map.fold subst ~init:typ ~f:(fun ~key ~data typ ->
      subst_typ key data typ)

let apply_scheme (subst: t) (scheme: Scheme.t) =
  Map.fold subst ~init:scheme ~f:(fun ~key ~data (typ, ftv) ->
      subst_typ key data typ, ftv)

let apply_kind (subst: t) (kind: Kind.t) =
  Map.fold subst ~init:kind ~f:(fun ~key ~data kind ->
      subst_kind key data kind)

let apply_env (subst: t) (env: Env.t) =
  Map.fold subst ~init:env ~f:(fun ~key ~data env ->
      Map.map env ~f:(subst_scheme key data))

let apply_name (subst: t) name =
  match Map.find subst name with
  | Some ({ contents = Variable name' }) -> name'
  | _ -> name

let merge (substl: t) (substr: t): t =
  let subst = Map.map substr ~f:(apply_typ substl) in
  Map.merge substl subst ~f:(fun ~key:_ -> function
      | `Both (_, right) -> Some right
      | `Right right -> Some right
      | `Left left -> Some left
    )
