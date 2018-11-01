open Ofluxl_std
open Ofluxl_types

type t =
  | Infinite of (Tvar.t * Type.Fixed.t)
  | MismatchedKinds of (Kind.Fixed.t * Kind.Fixed.t)
  | MismatchedTypes of (Type.Fixed.t * Type.Fixed.t)
  | InvalidSubst of (string * Type.Fixed.t)
  | InvalidTypeForKind of (Type.Fixed.t * Kind.Fixed.t)
  | InvalidKind of Kind.Fixed.t
  | InvalidType of Type.Fixed.t
  | UnknownIdentifier of string
  | UnknownRecordAccess of Set.M(String).t
[@@deriving sexp_of]

exception Infer of t [@@deriving sexp_of]

let throw err = raise @@ Infer err
