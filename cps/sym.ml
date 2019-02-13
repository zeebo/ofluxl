open Ofluxl_std

module Make () : sig
  type t
  val to_string : t -> string
  val fresh : unit -> t
  val eq : t -> t -> bool
end = struct
  let ctr = ref 0

  type t = string

  let to_string x = x

  let fresh () =
    ctr := !ctr + 1;
    "sym" ^ Int.to_string !ctr

  let eq x y = String.equal x y
end

include Make ()