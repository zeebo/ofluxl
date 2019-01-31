%token SEMICOLON
%token DOT COMMA COLON PIPE PIPE_ARROW RIGHT_PAREN_ARROW
%token LEFT_PAREN RIGHT_PAREN
%token LEFT_BRACKET RIGHT_BRACKET
%token LEFT_BRACE RIGHT_BRACE
%token EQUAL PLUS MINUS TIMES DIV
%token AND OR
%token RETURN WITH
%token <string> INTEGER FLOAT IDENT STRING DURATION TIME REGEX COMP
%token <char> CHAR
%token EOF

%nonassoc first
%nonassoc second
%nonassoc third
%left PLUS MINUS
%left TIMES DIV
%left AND OR
%left COMP
%right PIPE
%left LEFT_PAREN
%left LEFT_BRACKET DOT

%start <Ast.program> main

%%

separated_trailing_list(delim, X):
    | { [] }
    | x = X { [x] }
    | x = X delim xs = separated_trailing_list(delim, X) { x :: xs }

main:
    | p = program EOF { p }
    ;

program:
    | s = separated_trailing_list(SEMICOLON+, statement) { s }
    ;

statement:
    | i = IDENT EQUAL e = expr %prec first { Ast.Assign (i, e) }
    | e = expr %prec second { Ast.Expr e }
    ;

lambda:
    | LEFT_PAREN ps = separated_trailing_list(COMMA, func_param) RIGHT_PAREN_ARROW e = expr %prec third { Ast.Func (ps, [], e) }
    | LEFT_PAREN ps = separated_trailing_list(COMMA, func_param) RIGHT_PAREN_ARROW LEFT_BRACE p = list(statement); RETURN e = expr RIGHT_BRACE { Ast.Func (ps, p, e) }
    ;

call:
    | e = expr LEFT_PAREN args = separated_trailing_list(COMMA, colon_arg) RIGHT_PAREN { Ast.Call (e, args) }
    ;

value:
    | i = IDENT { Ast.Ident i }
    | i = INTEGER { Ast.Integer i }
    | f = FLOAT { Ast.Float f }
    | s = STRING { Ast.String s }
    | d = DURATION { Ast.Duration d }
    | t = TIME { Ast.Time t }
    | r = REGEX { Ast.Regex r }
    | c = CHAR { Ast.Char c }
    | LEFT_BRACKET es = separated_trailing_list(COMMA, expr) RIGHT_BRACKET { Ast.List es }
    | LEFT_BRACE vs = separated_trailing_list(COMMA, colon_arg) RIGHT_BRACE { Ast.Record vs }
    | LEFT_BRACE e = expr WITH vs = separated_trailing_list(COMMA, colon_arg) RIGHT_BRACE { Ast.With (e, vs) }
    ;

binary:
    | e1 = expr PLUS e2 = expr { Ast.Plus (e1, e2) }
    | e1 = expr MINUS e2 = expr { Ast.Minus (e1, e2) }
    | e1 = expr TIMES e2 = expr { Ast.Times (e1, e2) }
    | e1 = expr DIV e2 = expr { Ast.Div (e1, e2) }
    | e1 = expr PIPE e2 = expr { Ast.Pipe (e1, e2) }
    | e1 = expr c = COMP e2 = expr { Ast.Comp (e1, c, e2) }
    | e1 = expr AND e2 = expr { Ast.And (e1, e2) }
    | e1 = expr OR e2 = expr { Ast.Or (e1, e2) }
    ;

access:
    | e = expr DOT i = IDENT { Ast.Select (e, i) }
    | e = expr LEFT_BRACKET i = expr RIGHT_BRACKET { Ast.Index (e, i) }
    ;

group:
    | LEFT_PAREN e = expr RIGHT_PAREN { e }
    ;

expr:
    | l = lambda { l }
    | c = call   { c }
    | v = value  { v }
    | b = binary { b }
    | a = access { a }
    | g = group  { g }
    ;

colon_arg:
    | n = IDENT COLON e = expr { (n, e) }
    | n = IDENT { (n, Ast.Ident n) }
    | s = STRING COLON e = expr { (s, e) }
    ;

func_param:
    | n = IDENT d = func_param_default? { (n, d) }
    ;

func_param_default:
    | EQUAL PIPE_ARROW { Ast.DPipe }
    | EQUAL e = expr { Ast.DExpr e }
    ;
