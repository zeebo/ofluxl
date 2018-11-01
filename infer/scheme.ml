open Ofluxl_std
open Ofluxl_types

module Make(Tc: Tc.S) = struct
  module Type = Type.Make(Tc)

  type t = Type.t * Set.M(Tvar).t [@@deriving sexp_of]
  let empty typ: t = (typ, Set.empty (module Tvar))
  let make typ ftv: t = (typ, ftv)
  let mem ((_, ftv): t) tvar = Set.mem ftv tvar
end

module Fixed = Make(Tc.Identity)
include Make(Tc.Ref)
