open Ofluxl_std

module Tvar = Sym.Make (struct let kind = "tvar" end) ()

type typ =
  | Tvar