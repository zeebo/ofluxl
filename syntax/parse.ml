open Ofluxl_std

let lexbuf lexbuf =
  try Ok (Parser.main Lexer.token lexbuf) with
  | Lexer.Error _ -> Error (Err.Lexing lexbuf)
  | Parser.Error -> Error (Err.Parsing lexbuf)

let stdin () =
  lexbuf @@ Lexing.from_channel Stdio.stdin

let str str =
  lexbuf @@ Lexing.from_string str

