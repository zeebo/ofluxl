open Std

let parse cont lexbuf =
  try cont (Parser.main Lexer.token lexbuf) with
  | Lexer.Error _ -> Err.print_error @@ Err.Lexing lexbuf
  | Parser.Error -> Err.print_error @@ Err.Parsing lexbuf

let parse_stdin cont = Lexing.from_channel Stdio.stdin |> parse cont
let parse_str cont str = Lexing.from_string str |> parse cont

let print = printf "%s"

let sprint fn value = fn value |> Sexp.to_string_hum |> print
let print_expr = sprint Ast.sexp_of_expr
let print_typ = sprint Types.sexp_of_typ
let print_kind = sprint Types.sexp_of_kind

let solve expr =
  print_endline "ast:";
  print_expr expr;
  print_endline "\n";

  let typ, typ_constraints, kind_constraints = Solve.solve expr in

  print_endline "typ:";
  print_typ typ;
  print_endline "\n";

  List.iter typ_constraints ~f:(fun (left, right) ->
      print_endline "type constraint:";
      print_typ left;
      print " => ";
      print_typ right;
      print_endline "\n";
    );

  List.iter kind_constraints ~f:(fun (left, right) ->
      print_endline "kind constraint:";
      print_typ left;
      print " => ";
      print_kind right;
      print_endline "\n";
    )

let () =
  parse_stdin solve
