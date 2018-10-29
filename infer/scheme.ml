open Ofluxl_std
open Ofluxl_types

module Make(Tc: Tc.S) = struct
  module Type = Type.Make(Tc)

  type t = Type.t * Set.M(Tvar).t [@@deriving sexp_of]
  let empty typ = (typ, Set.empty (module Tvar))
  let mem tvar ((_, ftv): t) = Set.mem ftv tvar
end

module Fixed = Make(Tc.Identity)
include Make(Tc.Ref)
