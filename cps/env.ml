open Ofluxl_std

open Tvar

type t = Cps.ctm Map.M(Var).t

let set t x v =
  Map.set t ~key:x ~data:v

let lookup t x =
  Map.find t x