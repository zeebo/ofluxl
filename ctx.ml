open Std
open Types

type constr =
  | Type of typ
  | Kind of kind
[@@deriving sexp]

type t =
  { mutable constraints : (Tvar.t * constr) list
  ; mutable num: int
  ; kinds_index: kind list Hashtbl.M(Tvar).t
  }
[@@deriving sexp]

(* create an empty type inference context *)
let create (): t =
  { constraints = []
  ; num = 0
  ; kinds_index = Hashtbl.create (module Tvar)
  }

(* add a type constraint into the context *)
let add_typ_constraint ctx name typ =
  ctx.constraints <- (name, Type typ) :: ctx.constraints

(* add a kind constraint into the context *)
let add_kind_constraint ctx name kind =
  ctx.constraints <- (name, Kind kind) :: ctx.constraints;
  Hashtbl.update ctx.kinds_index name ~f:(function
      | Some kinds -> kind :: kinds
      | None -> [kind])

(* creates a new type and returns the tvar for it *)
let fresh_type ctx =
  ctx.num <- ctx.num + 1;
  sprintf "a%d" ctx.num

let empty_ftv = Set.empty (module Tvar)

(* TODO: i don't allow assignments inside of function bodies
 * which means that i don't have to consider the free type
 * variables of the environment when computing the free type
 * variables of some type. we need to subtract out all of
 * the free type variables in the environment if that changes
*)

(* compute the free type variables of a list of things. we define
 * this first because recursion can cause loss of generality.
*)
let ftv_list items ~f =
  List.map items ~f |> Set.union_list (module Tvar)

(* compute the free type variables in a type *)
let rec ftv_typ ctx = function
  | Basic _ -> empty_ftv
  | Invalid -> empty_ftv
  | List typ -> ftv_typ ctx typ

  | Variable name ->
    Set.add (
      match Hashtbl.find ctx.kinds_index name with
      | Some kinds -> ftv_list kinds ~f:(ftv_kind ctx)
      | None -> empty_ftv
    ) name

  | Func { args; ret; _ } ->
    ftv_list (Map.data args) ~f:(ftv_typ ctx)
    |> Set.union (ftv_typ ctx ret)

(* compute the free type variables in a kind *)
and ftv_kind ctx = function
  | KCls _ -> empty_ftv
  | KRecord { fields; _ } ->
    ftv_list (Map.data fields) ~f:(ftv_typ ctx)


(* instantiate a scheme. may add type and kind constraints. *)
let inst ctx (typ, ftv) =
  let subst = Set.fold ftv ~init:Subst.empty
      ~f:(fun subst name -> Subst.add subst name (fresh_type ctx))
  in
  List.iter ctx.constraints ~f:(fun (name, constr) ->
      match (Set.mem ftv name, constr) with
      | (true, Type typ) ->
        let name' = Subst.apply_name subst name in
        let typ' = Subst.apply_typ subst typ in
        add_typ_constraint ctx name' typ'
      | (true, Kind kind) ->
        let name' = Subst.apply_name subst name in
        let kind' = Subst.apply_kind subst kind in
        add_kind_constraint ctx name' kind'
      | _ -> ());
  Subst.apply_typ subst typ

(* walks and consumes all constraints added.
 * the callback is allowed to add more constraints
 * and they will be walked
*)
let rec apply ctx fn =
  match ctx.constraints with
  | [] -> ()
  | (name, constr) :: constrs ->
    ctx.constraints <- constrs;
    fn name constr;
    apply ctx fn
