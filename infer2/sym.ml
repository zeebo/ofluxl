open Ofluxl_std

module Make (K: sig val kind : string end) () : sig
  type t [@@deriving compare, sexp_of]
  include Comparator.S with type t := t

  val to_string : t -> string
  val fresh : unit -> t
  val eq : t -> t -> bool
end = struct
  module T = struct
    type t = string [@@deriving compare, sexp_of]

    let ctr = ref 0

    let to_string x = x

    let fresh () =
      ctr := !ctr + 1;
      K.kind ^ "_sym" ^ Int.to_string !ctr

    let eq x y = String.equal x y
  end

  include T
  include Comparator.Make(T)
end
