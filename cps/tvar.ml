open Ofluxl_std

module SymF (K: sig val kind : string end) () = struct
  type comparator_witness

  module U = Unionfind.Make ()
  type t = Sym.t U.t

  let to_string x = K.kind ^ "_" ^ Sym.to_string (U.unwrap x)
  let fresh () = U.wrap (Sym.fresh ())

  let find = U.find
  let union = U.union
  let same = U.same
end

module Var = SymF (struct let kind = "var" end) ()
module Cont = SymF (struct let kind = "cont" end) ()
module Tag = SymF (struct let kind = "tag" end) ()