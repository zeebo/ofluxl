open Err

open Ofluxl_std
open Ofluxl_types

type constr =
  | Type of Type.t * Type.t
  | Kind of Tvar.t * Kind.t
  | Inst of Subst.t
[@@deriving sexp_of]

(* t is some inference context. it is told the constraints and allows
 * the construction of type variables and enviornments. *)
type t =
  < kind_constraint: Type.t -> Kind.t -> unit
  ; typ_constraint: Type.t -> Type.t -> unit
  ; fresh_variable: Type.t
  ; fresh_name: Tvar.t
  ; generalize: Type.t -> Scheme.t
  ; insert: string -> Scheme.t -> unit
  ; resolve: string -> Type.t
  ; scope: Scheme.t Map.M(String).t -> (t -> Type.t) -> Type.t
  ; pop_constraint: constr option
  ; env: Env.t
  >

(* default constructs the default inference context *)
let default () = (object (self)

  (* inference state *)
  val mutable name = 0
  val mutable env = Env.default
  val mutable constraints = []

  (*
   * private methods
   *)

  (* instantiate creates a fresh type with all of the free type variables
   * of the scheme updated to be new types *)
  method instantiate scheme =
    let typ, ftv = scheme in

    (* create a substitution of the free type vars to fresh vars *)
    let subst = Set.fold ftv ~init:Subst.empty ~f:(fun subst name ->
        Subst.insert subst name self#fresh_variable)
    in

    (* add an instantiate constraint for the substitution *)
    constraints <- Inst subst :: constraints;

    (* return a type with the names substituted *)
    Type.substitute subst typ

  (*
   * public methods
   *)

  (* adds a kind constraint *)
  method kind_constraint typ kind =
    match typ with
    | Type.Variable name ->
      constraints <- Kind (name, kind) :: constraints
    | _ ->
      (* anything not a variable is concrete enough to have kinds implied
       * so there's no need to add a kind constraint for it *)
      if not @@ Kind.compatible_with_typ kind typ
      then throw @@ InvalidTypeForKind (typ, kind)
      else ()

  (* adds a type constraint *)
  method typ_constraint typl typr =
    constraints <- Type (typl, typr) :: constraints

  (* generates a fresh variable name *)
  method fresh_name =
    name <- name + 1;
    Tvar.of_string @@ sprintf "a%d" name

  (* generates a fresh variable type *)
  method fresh_variable =
    Type.Variable self#fresh_name

  (* generalize turns the typ into a scheme with the appropriate
   * captured free type variables *)
  method generalize typ =
    let eftv = Env.ftv env in
    let tftv = Type.ftv typ in
    let ftv = Set.diff tftv eftv in
    let ftv = match typ with
      | Variable name -> Set.remove ftv name
      | _ -> ftv
    in
    typ, ftv

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

  (* pop_constraint removes and returns a constraint *)
  method pop_constraint =
    match List.rev constraints with
    | [] -> None
    | c :: cs ->
      constraints <- cs;
      Some c

  (* env returns the current environment *)
  method env =
    env

end :> t)
