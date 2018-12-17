open Ofluxl_std
open Ofluxl_types

type t =
  | Infinite of (Tvar.t * Type.t)
  | MismatchedKinds of (Kind.t * Kind.t)
  | MismatchedTypes of (Type.t * Type.t)
  | InvalidSubst of (string * Type.t)
  | InvalidTypeForKind of (Type.t * Kind.t)
  | InvalidKind of Kind.t
  | InvalidType of Type.t
  | UnknownIdentifier of string
  | UnknownRecordAccess
[@@deriving sexp_of]

exception Infer of t [@@deriving sexp_of]

let throw err = raise @@ Infer err
