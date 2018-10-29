open Ofluxl_std

type t = Type.t * Set.M(Tvar).t [@@deriving sexp_of]

let empty typ = (typ, Set.empty (module Tvar))

let typ ((typ, _): t) = typ
let ftv ((_, ftv): t) = ftv

let mem tvar t = Set.mem (ftv t) tvar

