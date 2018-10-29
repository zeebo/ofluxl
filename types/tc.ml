open Ofluxl_std

(* S is the signature of a type constructor *)
module type S = sig
  type 'a t [@@deriving sexp_of, compare]
  val wrap: 'a -> 'a t
  val unwrap: 'a t -> 'a
end

module Ref = struct
  type 'a t = 'a ref [@@deriving sexp, compare]
  let wrap a = ref a
  let unwrap t = !t
end

module Identity = struct
  type 'a t = 'a [@@deriving sexp, compare]
  let wrap a = a
  let unwrap t = t
end
