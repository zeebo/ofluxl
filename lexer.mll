{
open Base
open Parser

exception Error of string * Lexing.position

let can_regex = ref true

let emit token =
    begin match token with
    | LEFT_PAREN
    | ARROW
    | ARROW_LEFT_BRACE
    | ARROW_LEFT_PAREN
    | COLON -> can_regex := true
    | _     -> can_regex := false
    end;
    token

let lexing_error lexbuf =
    let invalid_input = String.make 1 (Lexing.lexeme_char lexbuf 0) in
    raise (Error (invalid_input, lexbuf.Lexing.lex_curr_p))

let string_buffer = Buffer.create 256

let add_char c =
  Buffer.add_char string_buffer c

let add_lexeme lexbuf =
  Buffer.add_string string_buffer (Lexing.lexeme lexbuf)

let grab_string () =
  let str = Buffer.contents string_buffer in
  Buffer.reset string_buffer;
  str

let unquote lexeme =
    String.drop_prefix (String.drop_suffix lexeme 1) 1
}

let white   = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"

let digit  = ['0'-'9']
let digit2 = digit digit
let digit4 = digit2 digit2
let number = digit+
let ident  = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*

rule token = parse
    | white+  { token lexbuf }
    | newline { Lexing.new_line lexbuf; token lexbuf }

    (* symbols *)
    | ')' white* "=>" white* '{' { emit ARROW_LEFT_BRACE }
    | ')' white* "=>" white* '(' { emit ARROW_LEFT_PAREN }
    | ')' white* "=>"            { emit ARROW }

    | '.'  { emit DOT }
    | ','  { emit COMMA }
    | '('  { emit LEFT_PAREN }
    | ')'  { emit RIGHT_PAREN }
    | '['  { emit LEFT_BRACKET }
    | ']'  { emit RIGHT_BRACKET }
    | '{'  { emit LEFT_BRACE }
    | '}'  { emit RIGHT_BRACE }
    | '='  { emit EQUAL }
    | ':'  { emit COLON }
    | '+'  { emit PLUS }
    | '-'  { emit MINUS }
    | '*'  { emit TIMES }
    | "|>" { emit PIPE }

    (* regex/division support *)
    | '/' { if not !can_regex then emit DIV else
            let start = Lexing.lexeme_start_p lexbuf in
            let regex = read_regex lexbuf in
            lexbuf.lex_start_p <- start;
            emit (REGEX regex)
          }

    (* some literals *)
    | digit4 '-' digit2 '-' digit2 'T' digit2 ':' digit2 ':' digit2 'Z' { emit (TIME (Lexing.lexeme lexbuf)) }

    | (number 'h') (number 'm')? (number 's')? { emit (DURATION (Lexing.lexeme lexbuf)) }
    | (number 'h')? (number 'm') (number 's')? { emit (DURATION (Lexing.lexeme lexbuf)) }
    | (number 'h')? (number 'm')? (number 's') { emit (DURATION (Lexing.lexeme lexbuf)) }

    | number { emit (NUMBER (Lexing.lexeme lexbuf)) }

    | ident { emit (IDENT  (Lexing.lexeme lexbuf)) }

    | '"' { let start = Lexing.lexeme_start_p lexbuf in
            let string = read_string lexbuf in
            lexbuf.lex_start_p <- start;
            emit (STRING string)
          }

    (* eof *)
    | eof     { EOF }
    | _       { lexing_error lexbuf }

and read_string = parse
    | '"'                       { grab_string () }
    | '\\' '"'                  { add_char '"';      read_string lexbuf }
    | '\\' '\\'                 { add_char '\\';     read_string lexbuf }
    | [^ '\r' '\n' '"' '\\' ]+  { add_lexeme lexbuf; read_string lexbuf }
    | _                         { lexing_error lexbuf }
    | eof                       { raise (Error ("Unclosed string literal", lexbuf.Lexing.lex_curr_p)) }

and read_regex = parse
    | '/'                       { grab_string () }
    | '\\' '/'                  { add_char '/';      read_regex lexbuf }
    | '\\' '\\'                 { add_char '\\';     read_regex lexbuf }
    | [^ '\r' '\n' '/' '\\' ]+  { add_lexeme lexbuf; read_regex lexbuf }
    | _                         { lexing_error lexbuf }
    | eof                       { raise (Error ("Unclosed string literal", lexbuf.Lexing.lex_curr_p)) }
