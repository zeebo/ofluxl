(* S is the signature of a type constructor *)
module type S = sig
  type 'a t [@@deriving sexp_of, compare]
  val wrap: 'a -> 'a t
  val unwrap: 'a t -> 'a
end
