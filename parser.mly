%token DOT COMMA ARROW COLON PIPE
%token LEFT_PAREN RIGHT_PAREN
%token LEFT_BRACKET RIGHT_BRACKET
%token LEFT_BRACE RIGHT_BRACE
%token EQUAL PLUS MINUS TIMES DIV UMINUS
%token <string> NUMBER IDENT STRING
%token EOF

%left PIPE
%left PLUS MINUS
%left TIMES DIV
%left LEFT_BRACKET DOT
%nonassoc UMINUS
%left LEFT_PAREN

%start <Ast.expr> main

%%

main: e = expr EOF { e };

// deficiencies                      | example
// ----------------------------------|-------------------------
// assignment                        | foo = 2
// parenthesized funcs with 1 param  | (a) => { a * a }
// non-braced funcs                  | (a, b) => a * b
// records returned from funcs       | (a, b) => ({a=a, b=b})
// pipe function parameters          | i forget the syntax


expr:
    | p = IDENT ARROW LEFT_BRACE e = expr RIGHT_BRACE { Ast.Func ([p], e) }
    | LEFT_PAREN p = IDENT COMMA ps = separated_list(COMMA, IDENT) RIGHT_PAREN ARROW LEFT_BRACE e = expr RIGHT_BRACE { Ast.Func (p :: ps, e) }
    | i = IDENT { Ast.Ident i }
    | n = NUMBER { Ast.Number n }
    | s = STRING { Ast.String s }
    | e1 = expr PLUS  e2 = expr { Ast.Plus (e1, e2) }
    | e1 = expr MINUS e2 = expr { Ast.Minus (e1, e2) }
    | e1 = expr TIMES e2 = expr { Ast.Times (e1, e2) }
    | e1 = expr DIV e2 = expr { Ast.Div (e1, e2) }
    | MINUS e = expr %prec UMINUS { Ast.Uminus e }
    | e = expr LEFT_PAREN args = separated_list(COMMA, call_arg) RIGHT_PAREN { Ast.Call (e, args) }
    | e1 = expr PIPE e2 = expr  { Ast.Pipe (e1, e2) }
    | LEFT_BRACKET es = separated_list(COMMA, expr) RIGHT_BRACKET { Ast.List es }
    | LEFT_BRACE vs = separated_list(COMMA, record_value) RIGHT_BRACE { Ast.Record vs }
    | e = expr DOT i = IDENT { Ast.Select (e, i) }
    | e = expr LEFT_BRACKET i = expr RIGHT_BRACKET { Ast.Index (e, i) }
    | LEFT_PAREN e = expr RIGHT_PAREN { e }
    ;

call_arg:     n = IDENT COLON e = expr { (n, e) };
record_value: n = IDENT EQUAL e = expr { (n, e) };
