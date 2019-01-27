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
  ; solve: unifier -> Scheme.t Hashtbl.M(String).t * Kind.t Hashtbl.M(Tvar).t
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
    typ, Set.diff tftv eftv

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
  method solve _unifier =
    List.iter typ_constraints ~f:(fun (a, b) ->
      printf "%s <=> %s\n"
        (Sexp.to_string_hum (Type.sexp_of_t a))
        (Sexp.to_string_hum (Type.sexp_of_t b))
      );

    println "";

    List.iter kind_constraints ~f:(fun (a, b) ->
      printf "%s  => %s\n"
        (Tvar.to_string a)
        (Sexp.to_string_hum (Kind.sexp_of_t b))
      );

    throw UnknownRecordAccess
end :> t)
