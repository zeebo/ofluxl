open Ofluxl_std

type star
type row
type ('k1, 'k2) fn

type base =
  | Integer
[@@deriving compare]

type _ c =
  | Base : base -> star c
  | Fun : star c * star c -> star c
  | Row : row c
  | Ext : string * star c * row c -> row c
  | Rec : row c -> star c
  | Var : Tvar.t -> 'k c

type typ = star c

type pred = row c * string

type qual =
  | Typ of typ
  | Qual of pred list * typ

type scheme =
  | Qual of qual
  | Scheme of Set.M(Tvar).t * qual

type term =
  | Ident of string
  | App of term * term
  | Lam of string * term
  | Let of string * term * term

let rec flatten_row row =
  let sort_fields =
    List.sort ~compare:(fun (l1, _) (l2, _) ->
        String.compare l1 l2)
  in

  let rec helper acc = function
    | Row -> sort_fields acc, None
    | Var tvar -> sort_fields acc, Some tvar
    | Ext (label, typ, row) -> helper ((label, typ) :: acc) row
  in
  helper [] row

and fields_eq f1 f2 =
  match f1, f2 with
  | [], [] -> true
  | (l1, t1) :: f1, (l2, t2) :: f2 ->
    if String.equal l1 l2 && c_eq t1 t2
    then fields_eq f1 f2
    else false
  | _ -> false

and vars_eq v1 v2 =
  match v1, v2 with
  | Some tv1, Some tv2 -> Tvar.eq tv1 tv2
  | None, None -> true
  | _ -> false

and fields_contains f1 ~contains:f2 =
  match f1, f2 with
  | [], [] -> true
  | (l1, t1) :: f1', (l2, t2) :: f2' ->
    if String.equal l1 l2
    then c_eq t1 t2 && fields_eq f1' f2'
    else fields_eq f1' f2
  | _ -> false

and rows_eq r1 r2 =
  let f1, v1 = flatten_row r1
  and f2, v2 = flatten_row r2
  in vars_eq v1 v2 && fields_eq f1 f2

and c_eq (type a) (c1: a c) (c2: a c) =
  match (c1, c2) with
  | Base b1, Base b2 -> compare_base b1 b2 = 0
  | Fun (c1a, c1b), Fun (c2a, c2b) -> c_eq c1a c2a && c_eq c1b c2b
  | Row, Row -> true
  | Ext _, Ext _ -> rows_eq c1 c2
  | Rec r1, Rec r2 -> rows_eq r1 r2
  | Var tv1, Var tv2 -> Tvar.eq tv1 tv2
  | _ -> false

and pred_eq (r1, l1) (r2, l2) =
  String.equal l1 l2 && rows_eq r1 r2

and pred_empty = function
  | Row, _ -> true
  | _ -> false

and pred_entails preds (row, label) as pred =
  let fields, v = flatten_row row in

  empty_pred pred ||
  List.exists preds ~f:(pred_eq pred) ||
  List.exists preds ~f:(fun (row', label') ->
      if not @@ String.eq label label'
      then false
      else
        let fields', v' = flatten_row row' in
        vars_eq v v' && fields_contains fields' ~contains:fields)
