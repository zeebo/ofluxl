open Ofluxl_std

type t [@@deriving sexp_of, compare, hash]

include Comparator.S with type t := t

val equal: t -> t -> bool
val of_string: string -> t
val to_string: t -> string
