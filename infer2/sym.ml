open Ofluxl_std

module Make (K: sig val kind : string end) () : sig
  type comparator_witness (* can be used as map keys *)
  type t

  val to_string : t -> string
  val fresh : unit -> t
  val eq : t -> t -> bool
end = struct
  type comparator_witness
  type t = string

  let ctr = ref 0

  let to_string x =
    x

  let fresh () =
    ctr := !ctr + 1;
    K.kind ^ "_sym" ^ Int.to_string !ctr

  let eq x y =
    String.equal x y
end