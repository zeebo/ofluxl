open Err

open Ofluxl_std
open Ofluxl_types

(* t is some inference context. it is told the constraints and allows
 * the construction of type variables and enviornments. finally, it
 * holds the logic for driving solving.
*)
type t =
  < kind_constraint: Type.t -> Kind.kind -> unit
  ; typ_constraint: ?strict:bool -> Type.t -> Type.t -> unit
  ; fresh_variable: Type.t
  ; insert: string -> Type.t -> unit
  ; resolve: string -> Type.t
  ; scope: Scheme.t Map.M(String).t -> (t -> Type.t) -> Type.t
  ; solve: unifier -> Env.Fixed.t * Kind.Fixed.t Hashtbl.M(Tvar).t
  >

(* unifier holds unification logic, and is given a unification type
 * for issuing recursive unification requests
*)
and unifier =
  < typs: ut -> Type.t -> Type.t -> Type.typ
  ; kinds: ut -> Kind.t -> Kind.t -> Kind.kind
  >

(* ut is the interface that a unifier uses to perform recursive
 * unifications.
*)
and ut =
  < unify_typs: unifier -> ?strict:bool -> Type.t -> Type.t -> Type.t
  ; unify_kinds: unifier -> Kind.t -> Kind.t -> Kind.t
  >

(* default constructs the default inference context *)
let default () = (object (self)

  (* inference state *)
  val mutable name = 0
  val mutable env = Env.empty
  val mutable typ_constraints = Linked_queue.create ()
  val mutable kind_constraints = Linked_queue.create ()
  val kind_index = Hashtbl.create (module Tvar)

  (*
   * private methods
   *)

  (* add_kind_index allows the given kind to be looked up by the name
   * if the provided type is a variable
  *)
  method add_kind_index name kind =
    Hashtbl.update kind_index name ~f:(function
        | Some kinds -> kind :: kinds
        | None -> [kind])

  (* instantiate creates a fresh type with all of the free type variables
   * of the scheme updated to be new types
  *)
  method instantiate scheme =
    let typ, ftv = scheme in

    (* create a mapping of the type variable names *)
    let mapping = Hashtbl.create (module Tvar) in
    Set.iter ftv ~f:(fun name -> Hashtbl.set mapping ~key:name ~data:self#fresh_tvar);

    (* add kind constraints for new variable names *)
    Set.iter ftv ~f:(fun name ->
        match Hashtbl.find kind_index name with
        | None -> ()
        | Some kinds -> List.iter kinds ~f:(fun kind ->
            let name = Hashtbl.find_exn mapping name in
            let kind = Kind.substitute mapping kind in
            Linked_queue.enqueue kind_constraints (name, kind);
            self#add_kind_index name kind));

    (* return a type with the names substituted *)
    Type.substitute mapping typ

  (* generates a fresh tvar *)
  method fresh_tvar =
    name <- name + 1;
    Tvar.of_string @@ sprintf "a%d" name

  (*
   * public methods
   *)

  (* adds a kind constraint *)
  method kind_constraint typ kind =
    printf "constrain %s => %s\n"
      (sexp_sprint Type.sexp_of_t typ)
      (sexp_sprint Kind.sexp_of_kind kind);

    let kind = Kind.wrap kind in
    match Type.unwrap typ with
    | Variable name ->
      Linked_queue.enqueue kind_constraints (name, kind);
      self#add_kind_index name kind
    | _ ->
      (* anything not a variable is concrete enough to have kinds implied
       * so there's no need to add a kind constraint for it *)
      if not @@ Kind.compatible_with_typ kind typ
      then throw @@ InvalidTypeForKind (Fix.typ typ, Fix.kind kind)
      else ()

  (* adds a type constraint *)
  method typ_constraint ?(strict=true) typl typr =
    Linked_queue.enqueue typ_constraints (typl, typr, strict)

  (* generates a fresh variable type *)
  method fresh_variable =
    let name = self#fresh_tvar in
    let typ = Type.wrap @@ Variable name in
    typ

  (* insert permanently adds the generalized type
   * into the environment
  *)
  method insert ident typ =
    let ftv =
      Map.data env
      |> List.map ~f:(fun (_, ftv) -> ftv)
      |> Set.union_list (module Tvar)
    in
    let scheme = Scheme.make typ @@ Set.diff (Type.ftv typ) ftv in
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
    let kinds = Hashtbl.create (module Tvar) in

    (* we continue unification while we have any constraints *)
    while not @@ Linked_queue.is_empty typ_constraints ||
          not @@ Linked_queue.is_empty kind_constraints do

      (* unify all of the kind constraints into our kinds hash table *)
      while not @@ Linked_queue.is_empty kind_constraints do
        let name, kind = Linked_queue.dequeue_exn kind_constraints in

        Hashtbl.update kinds name ~f:(function
            | None -> kind
            | Some kind' -> self#unify_kinds unifier kind kind')
      done;

      (* unify all of the typ constraints *)
      while not @@ Linked_queue.is_empty typ_constraints do
        let left, right, strict = Linked_queue.dequeue_exn typ_constraints in

        (* perform the type unification, but only throw if strict *)
        let unify () = unifier#typs (self :> ut) left right in
        let typ = if strict then unify ()
          else try unify () with | Infer _ -> Invalid
        in

        (* if a type variable changed, update names everywhere *)
        begin match (Type.unwrap left, Type.unwrap right) with
          | Variable name, _ -> self#update_names kinds name typ
          | _, Variable name -> self#update_names kinds name typ
          | _ -> ()
        end;

        (* update the types now *)
        left := typ;
        right := typ
      done;

    done;

    Fix.env env, Hashtbl.map kinds ~f:Fix.kind

  (* update_names ensures that all of the references to the name are
   * either forwarded or removed from all of the state
  *)
  method update_names kinds name typ =
    printf "updating %s to %s\n" (Tvar.to_string name) (sexp_sprint Type.sexp_of_typ typ);

    (* add a kind constraint for the new type if there exists one under
     * the old name.
    *)
    begin match Hashtbl.find kinds name with
      | None -> ()
      | Some kind ->
        self#kind_constraint (Type.wrap typ) (Kind.unwrap kind);
        Hashtbl.remove kinds name
    end;

    (* update any free type variables in our environment to be the
     * new name, or delete them if they became concrete
    *)
    env <- Map.map env ~f:(fun (typ', ftv) ->
        match Set.mem ftv name, typ with
        | true, Variable name' ->
          (typ', Set.add (Set.remove ftv name) name')
        | _ -> (typ', Set.remove ftv name))

  (*
   * unification callbacks
   *)

  (* unify_typs creates a fresh variable to be the result of the unification
   * and adds constraints linking the variable to the correct type. actual
   * unification calls happen in solve.
  *)
  method unify_typs _ ?(strict=true) left right =
    let typ = self#fresh_variable in
    self#typ_constraint typ left;
    self#typ_constraint ~strict left right;
    typ

  (* unify_kinds unifies the given kinds and returns the result, updating
   * all of the typs and kinds with the unification.
  *)
  method unify_kinds unifier left right =
    let kind = unifier#kinds (self :> ut) left right in
    left := kind; right := kind;
    right

end :> t)
