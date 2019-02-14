open Ofluxl_std

open Tvar

let builtins =
  let m x = (x, Var.fresh ()) in
  Map.of_alist_exn (module String)
    [ m "add"
    ; m "sub"
    ; m "mul"
    ; m "div"
    ; m "neg"
    ; m "cmp"
    ; m "and"
    ; m "or"
    ; m "tern"
    ; m "sel"
    ]

let get name =
  Map.find_exn builtins name