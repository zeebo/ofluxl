open Ofluxl_std
open Ofluxl_syntax

module Integer = struct
  type t = int
  let to_t = Int.of_string
  let wrap t = Ast.Integer (Int.to_string t)
  let (+) = (+)
  let (-) = (-)
  let ( * ) = ( * )
  let (/) = (/)
  let neg t = -t
  let cmp = Int.compare
end

module String = struct
  type t = string
  let to_t t = t
  let wrap t = Ast.String t
  let (+) = (^)
  let cmp = String.compare
end

module Float = struct
  type t = float
  let to_t = Float.of_string
  let wrap t = Ast.Float (Float.to_string t)
  let (+) = (+.)
  let (-) = (-.)
  let ( * ) = ( *. )
  let (/) = (/.)
  let neg t = -1.0 *. t
  let cmp = Float.compare
end

module type Eval = sig
  type t
  val to_t: string -> t
  val wrap: t -> Ast.expr
end

module type Cmp = sig
  include Eval
  val cmp: t -> t -> int
end

module type Add = sig
  include Eval
  val (+): t -> t -> t
end

module type Sub = sig
  include Eval
  val (-): t -> t -> t
end

module type Mul = sig
  include Eval
  val ( * ): t -> t -> t
end

module type Div = sig
  include Eval
  val (/): t -> t -> t
end

module type Neg = sig
  include Eval
  val neg: t -> t
end

let add (module E: Add) l r =
  let open E in wrap (to_t l + to_t r)

let sub (module E: Sub) l r =
  let open E in wrap (to_t l - to_t r)

let mul (module E: Mul) l r =
  let open E in wrap (to_t l * to_t r)

let div (module E: Div) l r =
  let open E in wrap (to_t l / to_t r)

let neg (module E: Neg) v =
  let open E in wrap (neg (to_t v))

let eq (module E: Cmp) l r =
  let open E in Ast.Bool ((cmp (to_t l) (to_t r)) = 0)

let neq (module E: Cmp) l r =
  let open E in Ast.Bool ((cmp (to_t l) (to_t r)) <> 0)

let lt (module E: Cmp) l r =
  let open E in Ast.Bool ((cmp (to_t l) (to_t r)) < 0)

let gt (module E: Cmp) l r =
  let open E in Ast.Bool ((cmp (to_t l) (to_t r)) > 0)

let leq (module E: Cmp) l r =
  let open E in Ast.Bool ((cmp (to_t l) (to_t r)) <= 0)

let geq (module E: Cmp) l r =
  let open E in Ast.Bool ((cmp (to_t l) (to_t r)) >= 0)