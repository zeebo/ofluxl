let id x = x

type 'a pair = 'a * 'a

let mapP f =
  function
  | a, b -> (f a, f b)

type 'v incr =
  | Zero
  | Succ of 'v

let mapI f =
  function
  | Zero -> Zero
  | Succ x -> Succ (f x)

type 'v term =
  | Var of 'v
  | App of 'v term pair
  | Lam of 'v incr term

let rec mapT: 'a 'b. ('a -> 'b) -> 'a term -> 'b term =
  fun f -> function
    | Var x -> Var (f x)
    | App p -> App (mapP (mapT f) p)
    | Lam t -> Lam (mapT (mapI f) t)

module FoldT (F: sig
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

module GfoldT (F: sig
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

let rec kfoldT : 'a 'b. ('a -> 'b) -> ('b pair -> 'b) -> ('b -> 'b) -> ('a incr -> 'a) -> 'a term -> 'b =
  fun v a l k -> function
    | Var x -> v x
    | App p -> a (mapP (kfoldT v a l k) p)
    | Lam t -> l (kfoldT v a l k (mapT k t))

let showT =
  let showP (x, y) = "(" ^ x ^ " " ^ y ^ ")"
  and showI =
    function
    | Zero -> "0"
    | Succ x -> "S" ^ x
  in
  kfoldT id showP (fun l -> "L" ^ l) showI

let distT =
  function
  | Zero -> Var Zero
  | Succ x -> mapT (fun n -> Succ n) x

let joinT t =
  let module G = GfoldT (struct
      type 'a m = 'a term
      type 'a n = 'a term
      let v = id
      let a p = App p
      let l t = Lam t
      let k = distT
    end) in G.gfoldT t

let match_ x y =
  if x = y then Zero else Succ y

let abstract x t =
  Lam (mapT (match_ x) t)

let subst x =
  function
  | Zero -> x
  | Succ y -> y

let apply t t' =
  joinT (mapT (fun i -> subst t (mapI (fun v -> Var v) i)) t')
