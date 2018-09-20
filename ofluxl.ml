open Base
open Stdio

let parse lexbuf =
  try Ok (Parser.main Lexer.token lexbuf) with
  | Lexer.Error _ -> Error (Err.Lexing lexbuf)
  | Parser.Error -> Error (Err.Parsing lexbuf)

let parse_stdin () = Lexing.from_channel Stdio.stdin |> parse
let parse_str str = Lexing.from_string str |> parse

let print = function
  | Ok expr -> expr |> Ast.sexp_of_expr |> Sexp.to_string_hum |> print_endline
  | Error err -> err |> Err.print_error

(* pick one of these two *)


let () =
  parse_stdin () |> print


(*
let () =
  Js.export "ofluxl"
    (object%js
      method parse str = str |> Js.to_string |> parse_str |> print end
    )
*)
