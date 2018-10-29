open Ofluxl_std

type t = Type.t * Set.M(Tvar).t [@@deriving sexp_of]

let empty typ = (typ, Set.empty (module Tvar))
