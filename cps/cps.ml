open Ofluxl_std

exception Unimplemented

module Var = Unique.Make ()
module Cont = Unique.Make ()
module Tag = Unique.Make ()

type ctm =
  | LetVal of Var.t * cval * ctm
  | LetPi of Var.t * int * Var.t * ctm
  | LetCont of Cont.t * Var.t * ctm * ctm
  | ContApp of Cont.t * Var.t
  | App of Var.t * Cont.t * Var.t
  | Case of Var.t * (Tag.t * Cont.t) list
  | Halt of Var.t

and cval =
  | Void
  | Tuple of Var.t list
  | Tag of Tag.t * Var.t
  | Lam of Cont.t * Var.t * ctm

let rec show_ctm = function
  | LetVal (x, v, k) -> sprintf "letval x%s = (%s) in (%s)" (Var.to_string x) (show_cval v) (show_ctm k)
  | LetPi (x, n, z, k) -> sprintf "let x%s = pi%d x%s in (%s)" (Var.to_string x) n (Var.to_string z) (show_ctm k)
  | LetCont (k, x, k', l) -> sprintf "letcont k%s x%s = (%s) in (%s)" (Cont.to_string k) (Var.to_string x) (show_ctm k') (show_ctm l)
  | ContApp (k, x) -> sprintf "k%s x%s" (Cont.to_string k) (Var.to_string x)
  | App (f, k, x) -> sprintf "x%s k%s x%s" (Var.to_string f) (Cont.to_string k) (Var.to_string x)
  | Case (x, ks) -> sprintf "case x%s of %s" (Var.to_string x) (
      List.map ks ~f:(fun (t, k) -> sprintf "| t%s -> k%s" (Tag.to_string t) (Cont.to_string k))
      |> String.concat ~sep:" ")
  | Halt x -> sprintf "halt x%s" (Var.to_string x)

and show_cval = function
  | Void -> "()"
  | Tuple xs -> sprintf "(%s)" (List.map xs ~f:(fun x -> sprintf "x%s" (Var.to_string x)) |> String.concat ~sep:", ")
  | Tag (t, x) -> sprintf "t%s x%s" (Tag.to_string t) (Var.to_string x)
  | Lam (k, x, k') -> sprintf "\\k%s x%s. (%s)" (Cont.to_string k) (Var.to_string x) (show_ctm k')

let halt z = Halt z

type ml =
  | Var of Var.t
  | App of ml * ml
  | Lam of Var.t * ml
  | Tuple of ml list
  | Pi of int * ml
  | Void
  | Tag of Tag.t * ml
  | Let of Var.t * ml * ml
  | Case of ml * (Tag.t * Var.t * ml) list

let rec cps : ml -> (Var.t -> ctm) -> ctm =
  fun m k -> match m with
    | Var x ->
      k x

    | Void ->
      let x = Var.sym () in
      LetVal (x, Void, k x)

    | App (e1, e2) ->
      cps e1 (fun z1 ->
          cps e2 (fun z2 ->
              let x = Var.sym () in
              let k' = Cont.sym () in
              LetCont (k', x, k x, App (z1, k', z2))))

    | Tuple es ->
      let rec helper acc es =
        match es with
        | e :: es -> cps e (fun z -> helper (z :: acc) es)
        | [] ->
          let x = Var.sym () in
          LetVal (x, Tuple (List.rev acc), k x)
      in helper [] es

    | Tag (t, e) ->
      cps e (fun z ->
          let x = Var.sym () in
          LetVal (x, Tag (t, z), k x))

    | Pi (n, e) ->
      cps e (fun z ->
          let x = Var.sym () in
          LetPi (x, n, z, k x))

    | Lam (x, e) ->
      let f = Var.sym () in
      let k' = Cont.sym () in
      LetVal (f, Lam (k', x, cps' e k'), k f)

    | Let (x, e1, e2) ->
      let j = Cont.sym () in
      LetCont (j, x, cps e2 k, cps' e1 j)

    | Case (e, es) ->
      cps e (fun z ->
          let j = Cont.sym () in
          let rec helper acc es =
            match es with
            | (t, x, e) :: es ->
              let k' = Cont.sym () in
              LetCont (k', x, cps' e j, helper ((t, k') :: acc) es)
            | [] -> Case (z, List.rev acc)
          in
          let x = Var.sym () in
          LetCont (j, x, k x, helper [] es))

and cps' : ml -> Cont.t -> ctm  =
  fun m k -> match m with
    | Var x ->
      ContApp (k, x)

    | App (e1, e2) ->
      cps e1 (fun x1 ->
          cps e2 (fun x2 ->
              App (x1, k, x2)))

    | Lam (x ,e) ->
      let f = Var.sym () in
      let j = Cont.sym () in
      LetVal (f, Lam (j, x, cps' e j), ContApp (k, f))

    | Tuple es ->
      let rec helper acc es =
        match es with
        | e :: es -> cps e (fun z -> helper (z :: acc) es)
        | [] ->
          let x = Var.sym () in
          LetVal (x, Tuple (List.rev acc), ContApp (k, x))
      in helper [] es

    | Tag (t, e) ->
      cps e (fun z ->
          let x = Var.sym () in
          LetVal (x, Tag (t, z), ContApp (k, x)))

    | Void ->
      let x = Var.sym () in
      LetVal (x, Void, ContApp (k, x))

    | Pi (n, e) ->
      cps e (fun z ->
          let x = Var.sym () in
          LetPi (x, n, z, ContApp (k, x)))

    | Let (x, e1, e2) ->
      let j = Cont.sym () in
      LetCont (j, x, cps' e2 k, cps' e1 j)

    | Case (e, es) ->
      cps e (fun z ->
          let rec helper acc es =
            match es with
            | (t, x, e) :: es ->
              let k' = Cont.sym () in
              LetCont (k', x, cps' e k, helper ((t, k') :: acc) es)
            | [] -> Case (z, List.rev acc)
          in helper [] es)
