module Var = Sym.Make (struct let kind = "var" end) ()
module Cont = Sym.Make (struct let kind = "cont" end) ()
module Tag = Sym.Make (struct let kind = "tag" end) ()