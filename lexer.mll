{
open Base
open Parser

exception Error of string * Lexing.position

let emit token =
    token

let lexing_error lexbuf =
    let invalid_input = String.make 1 (Lexing.lexeme_char lexbuf 0) in
    raise (Error (invalid_input, lexbuf.Lexing.lex_curr_p))

let string_buffer = Buffer.create 256

let add_char c =
  Buffer.add_char string_buffer c

let add_lexeme lexbuf =
  Buffer.add_string string_buffer (Lexing.lexeme lexbuf)

let emit_string () =
  let str = Buffer.contents string_buffer in
  Buffer.reset string_buffer;
  emit (STRING str)

let unquote lexeme =
    String.drop_prefix (String.drop_suffix lexeme 1) 1
}

let white   = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"

let number  = ['0'-'9']+
let ident   = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*

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
    | '/'  { emit DIV }
    | "|>" { emit PIPE }

    (* some literals *)
    | (number 'h') (number 'm')? (number 's')? { emit (DURATION (Lexing.lexeme lexbuf)) }
    | (number 'h')? (number 'm') (number 's')? { emit (DURATION (Lexing.lexeme lexbuf)) }
    | (number 'h')? (number 'm')? (number 's') { emit (DURATION (Lexing.lexeme lexbuf)) }

    | number  { emit (NUMBER (Lexing.lexeme lexbuf)) }
    | ident   { emit (IDENT  (Lexing.lexeme lexbuf)) }
    | '"'     { let start = Lexing.lexeme_start_p lexbuf in
                let string = read_string lexbuf in
                lexbuf.lex_start_p <- start;
                string
              }

    (* eof *)
    | eof     { EOF }
    | _       { lexing_error lexbuf }

and read_string = parse
    | '"'                       { emit_string () }
    | '\\' '"'                  { add_char '"';      read_string lexbuf }
    | '\\' '\\'                 { add_char '\\';     read_string lexbuf }
    | [^ '\r' '\n' '\"' '\\' ]+ { add_lexeme lexbuf; read_string lexbuf }
    | _                         { lexing_error lexbuf }
    | eof                       { raise (Error ("Unclosed string literal", lexbuf.Lexing.lex_curr_p)) }
