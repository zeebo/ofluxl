open Ofluxl_std

module Make () : sig
  type 'a t

  val wrap : 'a -> 'a t
  val unwrap : 'a t -> 'a

  val find : 'a t -> 'a t
  val union : 'a t -> 'a t -> unit
  val same : 'a t -> 'a t -> bool
end = struct
  let ctr = ref 0

  type 'a t =
    { data: 'a
    ; id: int
    ; mutable rank: int
    ; mutable parent: 'a t
    }

  let wrap a =
    ctr := !ctr + 1;
    let rec v = { data = a; rank = 0; id = !ctr; parent = v } in
    v

  let unwrap { data; _ } =
    data

  let rec find t =
    if t.id = t.parent.id then t else begin
      t.parent <- t.parent.parent;
      find t.parent
    end

  let union x y =
    let xroot = find x
    and yroot = find y
    in

    if xroot.id = yroot.id then ()
    else begin
      let xroot, yroot = if xroot.rank < yroot.rank then
          yroot, xroot
        else
          xroot, yroot
      in

      yroot.parent <- xroot;
      if xroot.rank = yroot.rank then
        xroot.rank <- xroot.rank + 1
    end

  let same x y =
    (find x).id = (find y).id
end

include Make ()