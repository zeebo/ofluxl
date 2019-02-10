(* "de Bruijn notation as a nested datatype"
   Richard S. Bird
   Ross Paterson *)

(* The functions mapT, foldT, gfoldT, kfoldT, mapE and gfoldE are
   polymorphic-recursive *)

(* incr, term, p2 *)
type 'v incr = Zero | Succ of 'v
type 'v term = Var of 'v | App of ('v term * 'v term) | Lam of 'v incr term

type 'a pair = 'a * 'a

let id x = x

let mapP : ('a -> 'b) -> 'a pair -> 'b pair =
  fun f (x, y) -> (f x, f y)

(* mapI, mapT, p6 *)
let mapI : ('a -> 'b) -> 'a incr -> 'b incr = 
  fun f i -> match i with
    | Zero   -> Zero
    | Succ x -> Succ (f x)

let rec mapT : 'a 'b. ('a -> 'b) -> 'a term -> 'b term = 
  fun f t -> match t with
    | Var x -> Var (f x)
    | App p -> App (mapP (mapT f) p)
    | Lam t -> Lam (mapT (mapI f) t)

(* foldt, pp6-7.

   We need higher-kinded type variables here, so we must use a
   functor, at least for 'n'. (We could still keep v, a and l at term
   level if preferred.)
*)
module FoldT
  (F : 
    sig
      type 'a n
      val v : 'a -> 'a n
      val a : 'a n pair -> 'a n
      val l : 'a incr n -> 'a n
    end) =
struct
  let rec foldT : 'b. 'b term -> 'b F.n = 
    function
      | Var x -> F.v x
      | App p -> F.a (mapP foldT p)
      | Lam t -> F.l (foldT t)
end

(* gfoldT, p7  *)
module GfoldT
  (F : 
    sig
      type 'a m
      type 'a n
      val v : 'a m -> 'a n
      val a : 'a n pair -> 'a n
      val l : 'a incr n -> 'a n
      val k : 'a m incr -> 'a incr m
    end) =
struct
  let rec gfoldT : 'b. 'b F.m term -> 'b F.n =
    function
      | Var x -> F.v x
      | App p -> F.a (mapP gfoldT p)
      | Lam t -> F.l (gfoldT (mapT F.k t))
end

(* kfoldT, p8 *)
let rec kfoldT : 'a 'b. ('a -> 'b) -> ('b pair -> 'b) -> ('b -> 'b) ->
                        ('a incr -> 'a) ->
                        'a term -> 'b =
  fun v a l k t -> match t with
    | Var x -> v x
    | App p -> a (mapP (kfoldT v a l k) p)
    | Lam t -> l (kfoldT v a l k (mapT k t))

(* showT, p8 *)
let showT : string term -> string =
  let showP (x, y) = "("^ x ^" "^ y ^")"
  and showI = function
    | Zero   -> "0"
    | Succ x -> "S"^ x in
    kfoldT id showP (fun l -> "L"^l) showI


(* joinT, distT, p9 *)
let distT : 'a term incr -> 'a incr term =
  function
    | Zero -> Var Zero
    | Succ x -> mapT (fun n -> Succ n) x

let joinT (t : 'a term term) : 'a term =
  let module G =
    GfoldT (struct
              type 'a m = 'a term
              type 'a n = 'a term
              let v = id
              let a p = App p
              let l t = Lam t
              let k = distT
            end) in G.gfoldT t

(* abstract, match, p10 *)
let match_ : 'a -> 'a -> 'a incr =
  fun x y -> if x = y then Zero else Succ y

let abstract : 'a -> 'a term -> 'a term =
  fun x t -> Lam (mapT (match_ x) t)

(* apply, subst p11 *)
let subst : 'a -> 'a incr -> 'a =
  fun x i -> match i with
    | Zero   -> x
    | Succ y -> y

let apply : 'a term -> 'a incr term -> 'a term =
  fun t t' -> joinT (mapT (fun i -> subst t (mapI (fun v -> Var v) i)) t')

(* termE, p12 *)
type 'a termE = VarE of 'a
              | AppE of 'a termE pair
              | LamE of 'a termE incr termE

(* mapE, gfoldE p12 *)
let rec mapE : 'a 'b. ('a -> 'b) -> 'a termE -> 'b termE =
  fun f t -> match t with
    | VarE x -> VarE (f x)
    | AppE p -> AppE (mapP (mapE f) p)
    | LamE t -> LamE (mapE (mapI (mapE f)) t)

module GfoldE
  (F :
    sig
      type 'a m
      type 'a n
      val v :'a m -> 'a n
      val a : 'a n pair -> 'a n
      val l : 'a n incr n -> 'a n
      val k : 'a incr -> 'a incr m
    end) =
struct
  let rec gfoldE : 'b. 'b F.m termE -> 'b F.n =
    function 
      | VarE x -> F.v x
      | AppE p -> F.a (mapP gfoldE p)
      | LamE t -> F.l (gfoldE (mapE (fun e -> F.k (mapI gfoldE e)) t))
end

(* joinE, p13 *)
let joinE (t : 'a termE termE) : 'a termE =
  let module G =
    GfoldE
      (struct
         type 'a m = 'a termE
         type 'a n = 'a termE
         let v = id
         let a p = AppE p
         let l t = LamE t
         let k x = VarE x
       end) in G.gfoldE t

(* abstractE, applyE, p13 *)
let abstractE : 'a -> 'a termE -> 'a termE =
  fun x t -> LamE (mapE (fun e -> mapI (fun x -> VarE x) (match_ x e)) t)

let applyE : 'a termE -> 'a termE incr termE -> 'a termE =
  fun t t' -> joinE (mapE (subst t) t')
  
(* cvtE, pp13-14 *)
let cvtE (t : 'a termE) : 'a term =
  let module G =
    GfoldE 
      (struct 
         type 'a m = 'a
         type 'a n = 'a term
         let v x = Var x
         let a p = App p
         let l t = Lam (joinT (mapT distT t))
         let k = id
       end) in G.gfoldE t
