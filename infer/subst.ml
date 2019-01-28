open Ofluxl_std
open Ofluxl_types

type t = Type.t Map.M(Tvar).t [@@deriving sexp_of]

let empty: t =
  Map.empty (module Tvar)

let singleton =
  Map.singleton (module Tvar)

let insert t name typ =
  let s = singleton name typ in
  let t = Map.map t ~f:(fun typ -> Type.substitute s typ) in
  Map.set t ~key:name ~data:typ

let find_exn = Map.find_exn
let find = Map.find
let iter = Map.iter
let iteri = Map.iteri