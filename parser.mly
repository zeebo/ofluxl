%token DOT COMMA ARROW COLON PIPE
%token ARROW_LEFT_BRACE ARROW_LEFT_PAREN
%token LEFT_PAREN RIGHT_PAREN
%token LEFT_BRACKET RIGHT_BRACKET
%token LEFT_BRACE RIGHT_BRACE
%token EQUAL PLUS MINUS TIMES DIV UMINUS
%token <string> NUMBER IDENT STRING DURATION
%token EOF

%left PIPE
%left ARROW
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
// pipe function parameters          | i forget the syntax
// regex literals                    | /foo bar/
// time literals                     | 2018-05-22T19:53:26Z
// comparison operators              | a <= b

expr:
    | LEFT_PAREN ps = separated_list(COMMA, IDENT) ARROW_LEFT_BRACE e = expr RIGHT_BRACE { Ast.Func (ps, e) }
    | LEFT_PAREN ps = separated_list(COMMA, IDENT) ARROW_LEFT_PAREN e = expr RIGHT_PAREN { Ast.Func (ps, e) }
    | LEFT_PAREN ps = separated_list(COMMA, IDENT) ARROW e = expr { Ast.Func (ps, e) }
    | i = IDENT { Ast.Ident i }
    | n = NUMBER { Ast.Number n }
    | s = STRING { Ast.String s }
    | d = DURATION { Ast.Duration d }
    | e1 = expr PLUS  e2 = expr { Ast.Plus (e1, e2) }
    | e1 = expr MINUS e2 = expr { Ast.Minus (e1, e2) }
    | e1 = expr TIMES e2 = expr { Ast.Times (e1, e2) }
    | e1 = expr DIV e2 = expr { Ast.Div (e1, e2) }
    | MINUS e = expr %prec UMINUS { Ast.Uminus e }
    | e = expr LEFT_PAREN args = separated_list(COMMA, colon_arg) RIGHT_PAREN { Ast.Call (e, args) }
    | e1 = expr PIPE e2 = expr  { Ast.Pipe (e1, e2) }
    | LEFT_BRACKET es = separated_list(COMMA, expr) RIGHT_BRACKET { Ast.List es }
    | LEFT_BRACE vs = separated_list(COMMA, colon_arg) RIGHT_BRACE { Ast.Record vs }
    | e = expr DOT i = IDENT { Ast.Select (e, i) }
    | e = expr LEFT_BRACKET i = expr RIGHT_BRACKET { Ast.Index (e, i) }
    | LEFT_PAREN e = expr RIGHT_PAREN { e }
    ;

colon_arg:     n = IDENT COLON e = expr { (n, e) };
