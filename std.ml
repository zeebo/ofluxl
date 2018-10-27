include Base
include Stdio

let print = printf "%s"
let sprintf = Printf.sprintf

let sexp_sprint fn value = fn value |> Sexp.to_string_hum
let sexp_print fn value = sexp_sprint fn value |> print
let sexp_println fn value = sexp_sprint fn value |> print_endline
