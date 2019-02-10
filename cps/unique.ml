open Ofluxl_std

module Make () : sig
  type t = T of int
  val sym: unit -> t
  val to_string: t -> string
end = struct
  type t = T of int
  let ctr = ref 0
  let sym () =
    ctr := !ctr + 1;
    T !ctr
  let to_string (T n) = Int.to_string n
end