open Err

open Ofluxl_std
open Ofluxl_types

(* t is some inference context. it is told the constraints and allows
 * the construction of type variables and enviornments. finally, it
 * holds the logic for driving solving. *)
type t =
  < kind_constraint: Type.t -> Kind.t -> unit
  ; typ_constraint: Type.t -> Type.t -> unit
  ; fresh_variable: Type.t
  ; generalize: Type.t -> Scheme.t
  ; insert: string -> Scheme.t -> unit
  ; resolve: string -> Type.t
  ; scope: Scheme.t Map.M(String).t -> (t -> Type.t) -> Type.t
  ; solve: unifier -> Env.t * Kind.t Hashtbl.M(Tvar).t
  >

(* unifier holds unification logic, and is given a unification type
 * for issuing recursive unification requests *)
and unifier =
  < typs: ut -> Type.t -> Type.t -> Type.t
  ; kinds: ut -> Kind.t -> Kind.t -> Kind.t
  >

(* ut is the interface that a unifier uses to perform recursive
 * unifications. *)
and ut =
  < unify_typs: unifier -> Type.t -> Type.t -> Type.t
  ; unify_kinds: unifier -> Kind.t -> Kind.t -> Kind.t
  >

(* default constructs the default inference context *)
let default () = (object (self)

  (* inference state *)
  val mutable name = 0
  val mutable env = Env.default
  val mutable typ_constraints = []
  val mutable kind_constraints = []
  val kind_index = Hashtbl.create (module Tvar)

  (*
   * private methods
   *)

  (* add_kind_index allows the given kind to be looked up by the name
   * if the provided type is a variable *)
  method add_kind_index name kind =
    Hashtbl.update kind_index name ~f:(function
        | Some kinds -> kind :: kinds
        | None -> [kind])

  (* instantiate creates a fresh type with all of the free type variables
   * of the scheme updated to be new types *)
  method instantiate scheme =
    let typ, ftv = scheme in

    (* create a mapping of the type variable names *)
    let mapping = Hashtbl.create (module Tvar) in
    Set.iter ftv ~f:(fun name -> Hashtbl.set mapping ~key:name ~data:self#fresh_variable);

    (* add kind constraints for new variable names *)
    Set.iter ftv ~f:(fun name ->
        match Hashtbl.find kind_index name with
        | None -> ()
        | Some kinds -> List.iter kinds ~f:(fun kind ->
            let typ = Hashtbl.find_exn mapping name in
            let kind = Kind.substitute mapping kind in
            self#kind_constraint typ kind));

    (* return a type with the names substituted *)
    Type.substitute mapping typ

  (*
   * public methods
   *)

  (* adds a kind constraint *)
  method kind_constraint typ kind =
    match typ with
    | Variable name ->
      kind_constraints <- (name, kind) :: kind_constraints;
      self#add_kind_index name kind
    | _ ->
      (* anything not a variable is concrete enough to have kinds implied
       * so there's no need to add a kind constraint for it *)
      if not @@ Kind.compatible_with_typ kind typ
      then throw @@ InvalidTypeForKind (typ, kind)
      else ()

  (* adds a type constraint *)
  method typ_constraint typl typr =
    typ_constraints <- (typl, typr) :: typ_constraints

  (* generates a fresh variable type *)
  method fresh_variable =
    name <- name + 1;
    Type.Variable (Tvar.of_string @@ sprintf "a%d" name)

  (* generalize turns the typ into a scheme with the appropriate
   * captured free type variables *)
  method generalize typ =
    let eftv = Env.ftv env in
    let tftv = Type.ftv typ in
    let kinds = Set.fold ~init:[] tftv ~f:(fun accum name ->
        match Hashtbl.find kind_index name with
        | Some kinds -> List.append accum kinds
        | None -> accum )
    in
    let kftv = List.map kinds ~f:Kind.ftv in
    let ftv = Set.union_list (module Tvar) kftv |> Set.union tftv in
    typ, Set.diff ftv eftv

  (* insert permanently adds the generalized type
   * into the environment *)
  method insert ident scheme =
    env <- Env.set env ident scheme

  (* resolve finds and instantiates an identifier *)
  method resolve ident =
    match Env.find env ident with
    | Some scheme -> self#instantiate scheme
    | None -> throw @@ UnknownIdentifier ident

  (* scope runs the continuation with the extra entries in scope *)
  method scope entries cont =
    let saved = env in
    env <- Env.merge env entries;
    let typ = cont (self :> t) in
    env <- saved;
    typ

  (* solve checks all of the constraints and solves them *)
  method solve unifier =
    let ut = (object (self)
      val kinds = Hashtbl.create (module Tvar)
      val mapping = Hashtbl.create (module Tvar)
      val mutable env = env

      (* result returns the typed environment and relevant kinds *)
      method result =
        env, Hashtbl.filter_keys kinds ~f:(Set.mem @@ Env.ftv env)

      (* tvar_substitute is a helper to apply the substitution mapping
       * to the type variable *)
      method tvar_substitute mapping name =
        match Hashtbl.find mapping name with
        | None -> name
        | Some typ -> match typ with
          | Type.Variable name -> name
          | _ -> name

      method update_name unifier name typ =
        (* store the mapping first *)
        Hashtbl.set mapping ~key:name ~data:typ;

        (* if it maps to a new name, then copy and merge the kinds *)
        begin match typ with
          | Type.Variable name' -> begin
              let kind = match Hashtbl.find kinds name, Hashtbl.find kinds name' with
                | Some kind, Some kind' -> Some (self#unify_kinds unifier kind kind')
                | Some kind, _ -> Some kind
                | _, Some kind -> Some kind
                | _ -> None
              in
              match kind with
              | Some kind -> Hashtbl.set kinds ~key:name' ~data:kind
              | None -> ()
            end
          | _ -> ()
        end;

        (* update all of the kinds to include the mapping *)
        Hashtbl.mapi_inplace kinds ~f:(fun ~key ~data ->
            if Tvar.equal key name
            then Kind.substitute mapping data
            else data);

        (* update the environment to include the mapping *)
        env <- Env.substitute mapping env

      (* add_kind introduces the kind constraint under the name *)
      method add_kind unifier name kind =
        let name = self#tvar_substitute mapping name in
        Hashtbl.update kinds name ~f:(function
            | None -> kind
            | Some kind' -> self#unify_kinds unifier kind kind')

      (* unify_typs asks the unifier what the two types unify as, and
       * then updates the state with the results of that unification *)
      method unify_typs unifier left right =
        let left = Type.substitute mapping left in
        let right = Type.substitute mapping right in
        let typ = unifier#typs (self :> ut) left right in
        begin match left, right with
          | Variable name, _ -> self#update_name unifier name typ
          | _, Variable name -> self#update_name unifier name typ
          | _ -> ()
        end;
        typ

      (* unify_kinds asks the unifier what the two kinds unify as, and
       * then updates the state with the result of that unification *)
      method unify_kinds unifier left right =
        let left = Kind.substitute mapping left in
        let right = Kind.substitute mapping right in
        unifier#kinds (self :> ut) left right
    end) in

    List.iter kind_constraints ~f:(fun (name, kind) -> ut#add_kind unifier name kind);
    List.iter typ_constraints ~f:(fun (left, right) -> ignore @@ ut#unify_typs unifier left right);
    ut#result

end :> t)
