{
open Std
open Parser

exception Error of string * Lexing.position

let can_regex = ref true

let emit token =
    begin match token with
    | LEFT_PAREN
    | RIGHT_PAREN_ARROW
    | EQUAL
    | COMP "=~" (* maybe this should be any comparison *)
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
}

let white = ' ' | '\t'
let newline = "\r\n" | '\r' | '\n'

let digit  = ['0'-'9']
let digit2 = digit digit
let digit4 = digit2 digit2
let integer = digit+
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?
let ident  = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*

rule token = parse
    (* comments *)
    | "//" [^ '\r' '\n' ]* newline? { token lexbuf }
    | "/*" { read_comment lexbuf; token lexbuf }

    (* whitespace *)
    | white+  { token lexbuf }
    | newline { Lexing.new_line lexbuf; token lexbuf }

    (* arrow symbols *)
    | ')' white* "=>" white* '{' { emit RIGHT_PAREN_ARROW_LEFT_BRACE }
    | ')' white* "=>"            { emit RIGHT_PAREN_ARROW }
    | "<-"                       { emit PIPE_ARROW }

    (* comparisons *)
    | "==" | "!=" | "<=" | ">=" | '<' | '>' | "=~" { emit (COMP (Lexing.lexeme lexbuf))  }
    | ['a''A'] ['n''N'] ['d''D'] { emit AND }
    | ['o''O'] ['r''R'] { emit OR }

    (* symbols *)
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
    | ';'  { emit SEMICOLON }

    (* operators *)
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

    (* return is weird *)
    | "return" { RETURN }

    (* literals *)
    | digit4 '-' digit2 '-' digit2 'T' digit2 ':' digit2 ':' digit2 ('.' digit+)? 'Z' { emit (TIME (Lexing.lexeme lexbuf)) }
    | (integer 'h')  (integer 'm')? (integer 's')? (integer "ms")? (integer "us")? (integer "ns")? { emit (DURATION (Lexing.lexeme lexbuf)) }
    | (integer 'h')? (integer 'm')  (integer 's')? (integer "ms")? (integer "us")? (integer "ns")? { emit (DURATION (Lexing.lexeme lexbuf)) }
    | (integer 'h')? (integer 'm')? (integer 's')  (integer "ms")? (integer "us")? (integer "ns")? { emit (DURATION (Lexing.lexeme lexbuf)) }
    | (integer 'h')? (integer 'm')? (integer 's')? (integer "ms")  (integer "us")? (integer "ns")? { emit (DURATION (Lexing.lexeme lexbuf)) }
    | (integer 'h')? (integer 'm')? (integer 's')? (integer "ms")? (integer "us")  (integer "ns")? { emit (DURATION (Lexing.lexeme lexbuf)) }
    | (integer 'h')? (integer 'm')? (integer 's')? (integer "ms")? (integer "us")? (integer "ns")  { emit (DURATION (Lexing.lexeme lexbuf)) }
    | integer { emit (INTEGER (Lexing.lexeme lexbuf)) }
    | float   { emit (FLOAT (Lexing.lexeme lexbuf)) }
    | ident { emit (IDENT (Lexing.lexeme lexbuf)) }
    | '\'' { let start = Lexing.lexeme_start_p lexbuf in
             let char = read_char lexbuf in
             lexbuf.lex_start_p <- start;
             emit (CHAR char)
           }
    | '"' { let start = Lexing.lexeme_start_p lexbuf in
            let string = read_string lexbuf in
            lexbuf.lex_start_p <- start;
            emit (STRING string)
          }

    (* everything else *)
    | eof      { EOF }
    | _        { lexing_error lexbuf }

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

and read_char = parse
    | "\\t'"  { '\t' }
    | "\\n'"  { '\n' }
    | "\\r'"  { '\r' }
    | "\\\\'" { '\\' }
    | "\\''"  { '\'' }
    | _ '\''  { Lexing.lexeme_char lexbuf 0 }
    | _       { lexing_error lexbuf }
    | eof     { raise (Error ("Unclosed character literal", lexbuf.Lexing.lex_curr_p)) }

and read_comment = parse
    | "*/"    { }
    | newline { Lexing.new_line lexbuf; read_comment lexbuf }
    | _       { read_comment lexbuf }
