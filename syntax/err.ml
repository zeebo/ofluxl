open Ofluxl_std

type t =
  | Lexing of Lexing.lexbuf
  | Parsing of Lexing.lexbuf

let positions (lexbuf: Lexing.lexbuf) =
  let start = lexbuf.lex_start_p in
  let curr = lexbuf.lex_curr_p in
  start.pos_lnum,
  start.pos_cnum - start.pos_bol,
  curr.pos_cnum - curr.pos_bol

let print_context (lexbuf: Lexing.lexbuf) line =
  let lines = lexbuf.lex_buffer  |> Bytes.to_string |> String.split_lines in
  (* TODO(jeff): this traverses the list 3 times for no reason lol *)
  for n = line - 3 to line - 1 do
    match List.nth lines n with
    | Some line -> printf "%4d: %s\n" (n + 1) line
    | None -> ()
  done

let print_error error =
  let lexbuf, kind = match error with
    | Lexing lexbuf -> lexbuf, "syntax"
    | Parsing lexbuf -> lexbuf, "parse"
  in
  let line, s_col, c_col = positions lexbuf in
  let carrots = c_col - s_col in
  if carrots = 0 then
    printf "%s error: unexpected EOF\n" kind
  else begin
    printf "%s error:\n\n" kind;
    print_context lexbuf line;
    printf "%s%s%s\n"
      (String.make 6 ' ')
      (String.make s_col '~')
      (String.make carrots '^')
  end
