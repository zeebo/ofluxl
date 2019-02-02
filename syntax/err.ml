open Ofluxl_std

type t =
  | Lexing of Lexing.lexbuf
  | Parsing of Lexing.lexbuf

exception SyntaxErr of t

let positions (lexbuf: Lexing.lexbuf) =
  let start = lexbuf.lex_start_p in
  let curr = lexbuf.lex_curr_p in
  start.pos_lnum,
  start.pos_cnum - start.pos_bol,
  curr.pos_cnum - curr.pos_bol

let context_to_string (lexbuf: Lexing.lexbuf) line =
  let t = tracker () in
  let lines = lexbuf.lex_buffer  |> Bytes.to_string |> String.split_lines in
  (* TODO(jeff): this traverses the list 3 times for no reason lol *)
  for n = line - 3 to line - 1 do
    match List.nth lines n with
    | Some line -> t#add @@ sprintf "%4d: %s" (n + 1) line
    | None -> ()
  done;
  t#finish

let print_context (lexbuf: Lexing.lexbuf) line =
  print_endline @@ context_to_string lexbuf line

let to_string error =
  let t= tracker () in
  let lexbuf, kind = match error with
    | Lexing lexbuf -> lexbuf, "syntax"
    | Parsing lexbuf -> lexbuf, "parse"
  in
  let line, s_col, c_col = positions lexbuf in
  let carrots = c_col - s_col in
  if carrots = 0 then
    t#add @@ sprintf "%s error: unexpected EOF" kind
  else begin
    t#add @@ sprintf "%s error:\n" kind;
    t#add @@ context_to_string lexbuf line;
    t#add @@ sprintf "%s%s%s"
      (String.make 6 ' ')
      (String.make s_col '~')
      (String.make carrots '^')
  end;
  t#finish

let print error =
  print_endline @@ to_string error