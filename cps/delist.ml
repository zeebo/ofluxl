open Ofluxl_std

module Make () : sig
  type 'a t

  val wrap : 'a -> 'a t
  val unwrap : 'a t -> 'a

  type count =
    | One
    | Many

  val count : 'a t -> count
  val merge : 'a t -> 'a t -> unit
end = struct
  let ctr = ref 0

  type 'a t =
    { data: 'a
    ; id: int
    ; mutable next: 'a t
    ; mutable prev: 'a t
    }

  let wrap a =
    ctr := !ctr + 1;
    let rec v = { data = a; id = !ctr; next = v; prev = v } in
    v

  let unwrap { data; _ } =
    data

  type count =
    | One
    | Many

  let count t =
    if t.id = t.next.id
    then One
    else Many

  let merge x y =
    x.next.prev <- y.prev;
    y.prev.next <- x.next;
    x.next <- y;
    y.prev <- x;

end