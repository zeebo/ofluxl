open Ofluxl_std

open Tvar
open Cps

exception Unimplemented

module Env = struct
  type t = Cps.cval Map.M(Var).t

  let set (t: t) x v =
    Map.set t ~key:x ~data:v

  let lookup t x =
    Map.find t x
end

module Graph = struct
  type occurs =
    | Zero
    | One
    | Many

  let occurs _x =
    Zero

  let union _x _y =
    ()
end

(* TODO: see if we can make these mutable or something *)

let rec simp env =
  function
  (* Dead-Val *)
  | LetVal (x, v, l) ->
    begin match Graph.occurs x with
      | Zero -> simp env l
      | _ -> LetVal (x, simpVal env v, simp (Env.set env x v) l)
    end

  (* B-Tuple *)
  | LetPi (x, n, y, l) ->
    let failure () = LetPi (x, n, y, simp env l) in
    begin match Env.lookup env y with
      | Some (Tuple vs) ->
        begin match List.nth vs n with
          | Some z ->
            Graph.union x z;
            simp env l
          | None -> failure ()
        end
      | _ -> failure ()
    end

  | _ -> raise Unimplemented

and simpVal _env =
  function
  | _ -> raise Unimplemented