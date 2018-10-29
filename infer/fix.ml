open Ofluxl_std
open Ofluxl_types

(* maps from one type constructor to the other *)
module Make (From: Tc.S) (To: Tc.S) = struct
  let typ, kind, scheme, env =
    let module Make (Tc: Tc.S) = struct
      module Type = Type.Make(Tc)
      module Kind = Kind.Make(Tc)
      module Scheme = Scheme.Make(Tc)
      module Env = Env.Make(Tc)
    end in

    let module From = Make(From) in
    let module To = Make(To) in

    let rec typ from = To.Type.wrap @@
      match From.Type.unwrap from with
      | Invalid -> Invalid
      | List typ' -> List (typ typ')
      | Func { args; table; required; ret } ->
        Func { args = Map.map args ~f:typ ; table ; required; ret = typ ret }
      | Variable tvar -> Variable tvar
      | Basic Integer -> Basic Integer
      | Basic Float -> Basic Float
      | Basic Duration -> Basic Duration
      | Basic Time -> Basic Time
      | Basic Regex -> Basic Regex
      | Basic Char -> Basic Char
      | Basic String -> Basic String
      | Basic Bool -> Basic Bool
      | Basic Table -> Basic Table

    and kind from = To.Kind.wrap @@
      match From.Kind.unwrap from with
      | Cls Cmp -> Cls Cmp
      | Cls Add -> Cls Add
      | Cls Num -> Cls Num
      | Record { fields; upper; lower } ->
        Record { fields = Map.map fields ~f:typ; upper; lower }

    and scheme ((typ', ftv): From.Scheme.t): To.Scheme.t =
      (typ typ'), ftv

    and env (from: From.Env.t): To.Env.t =
      Map.map from ~f:scheme
    in

    typ, kind, scheme, env
end

include Make(Tc.Ref)(Tc.Identity)
