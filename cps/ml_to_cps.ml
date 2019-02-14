open Ofluxl_std

open Tvar
open Ml
open Cps

let rec conv : ml -> (Var.t -> ctm) -> ctm =
  fun m k -> match m with
    | Var x ->
      k x

    | Prim p ->
      let x = Var.fresh () in
      LetVal (x, Prim p, k x)

    | App (e1, e2) ->
      conv e1 (fun z1 ->
          conv e2 (fun z2 ->
              let x = Var.fresh () in
              let k' = Cont.fresh () in
              LetCont (k', x, k x, App (z1, k', z2))))

    | Tuple es ->
      let rec helper acc es =
        match es with
        | e :: es -> conv e (fun z -> helper (z :: acc) es)
        | [] ->
          let x = Var.fresh () in
          LetVal (x, Tuple (List.rev acc), k x)
      in helper [] es

    | Tag (t, e) ->
      conv e (fun z ->
          let x = Var.fresh () in
          LetVal (x, Tag (t, z), k x))

    | Pi (n, e) ->
      conv e (fun z ->
          let x = Var.fresh () in
          LetPi (x, n, z, k x))

    | Lam (x, e) ->
      let f = Var.fresh () in
      let k' = Cont.fresh () in
      LetVal (f, Lam (k', x, conv' e k'), k f)

    | Let (x, e1, e2) ->
      let j = Cont.fresh () in
      LetCont (j, x, conv e2 k, conv' e1 j)

    | Case (e, es) ->
      conv e (fun z ->
          let j = Cont.fresh () in
          let rec helper acc es =
            match es with
            | (t, x, e) :: es ->
              let k' = Cont.fresh () in
              LetCont (k', x, conv' e j, helper ((t, k') :: acc) es)
            | [] -> Case (z, List.rev acc)
          in
          let x = Var.fresh () in
          LetCont (j, x, k x, helper [] es))

and conv' : ml -> Cont.t -> ctm  =
  fun m k -> match m with
    | Var x ->
      ContApp (k, x)

    | App (e1, e2) ->
      conv e1 (fun x1 ->
          conv e2 (fun x2 ->
              App (x1, k, x2)))

    | Lam (x ,e) ->
      let f = Var.fresh () in
      let j = Cont.fresh () in
      LetVal (f, Lam (j, x, conv' e j), ContApp (k, f))

    | Tuple es ->
      let rec helper acc es =
        match es with
        | e :: es -> conv e (fun z -> helper (z :: acc) es)
        | [] ->
          let x = Var.fresh () in
          LetVal (x, Tuple (List.rev acc), ContApp (k, x))
      in helper [] es

    | Tag (t, e) ->
      conv e (fun z ->
          let x = Var.fresh () in
          LetVal (x, Tag (t, z), ContApp (k, x)))

    | Prim p ->
      let x = Var.fresh () in
      LetVal (x, Prim p, ContApp (k, x))

    | Pi (n, e) ->
      conv e (fun z ->
          let x = Var.fresh () in
          LetPi (x, n, z, ContApp (k, x)))

    | Let (x, e1, e2) ->
      let j = Cont.fresh () in
      LetCont (j, x, conv' e2 k, conv' e1 j)

    | Case (e, es) ->
      conv e (fun z ->
          let rec helper acc es =
            match es with
            | (t, x, e) :: es ->
              let k' = Cont.fresh () in
              LetCont (k', x, conv' e k, helper ((t, k') :: acc) es)
            | [] -> Case (z, List.rev acc)
          in helper [] es)