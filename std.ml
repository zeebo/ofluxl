include Base
include Stdio

let sprintf = Printf.sprintf
let sprintln format = Printf.sprintf (format Caml.(^^) "\n")

let print = printf "%s"
let println = printf "%s\n"

let sexp_sprint fn value = fn value |> Sexp.to_string_hum
let sexp_print fn value = sexp_sprint fn value |> print
let sexp_println fn value = sexp_sprint fn value |> println
