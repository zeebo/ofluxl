open Ofluxl_std

let lexbuf lexbuf =
  try Ok (Parser.main Lexer.token lexbuf) with
  | Lexer.Error _ -> Error (Err.Lexing lexbuf)
  | Parser.Error -> Error (Err.Parsing lexbuf)

let stdin () =
  lexbuf @@ Lexing.from_channel Stdio.stdin

let str str =
  lexbuf @@ Lexing.from_string str

let lexbuf_exn lexbuf =
  Parser.main Lexer.token lexbuf

let stdin_exn () =
  lexbuf_exn @@ Lexing.from_channel Stdio.stdin

let str_exn str =
  lexbuf_exn @@ Lexing.from_string str
