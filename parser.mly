%token DOT COMMA COLON PIPE PIPE_ARROW
%token RIGHT_PAREN_ARROW RIGHT_PAREN_ARROW_LEFT_BRACE
%token LEFT_PAREN RIGHT_PAREN
%token LEFT_BRACKET RIGHT_BRACKET
%token LEFT_BRACE RIGHT_BRACE
%token EQUAL PLUS MINUS TIMES DIV UMINUS
%token AND OR
%token RETURN
%token <string> INTEGER FLOAT IDENT STRING DURATION TIME REGEX COMP
%token <char> CHAR
%token EOF

%left PLUS MINUS
%left TIMES DIV
%left COMP
%left AND OR
%left LEFT_PAREN
%left RIGHT_PAREN_ARROW
%nonassoc UMINUS
%left LEFT_BRACKET DOT
%left EQUAL
%right PIPE
%left RETURN

%start <Ast.expr> main

%%

main: e = expr EOF { e };

expr:
    | LEFT_PAREN ps = separated_list(COMMA, func_param) RIGHT_PAREN_ARROW e = expr { Ast.Func (ps, e) }
    | LEFT_PAREN ps = separated_list(COMMA, func_param) RIGHT_PAREN_ARROW_LEFT_BRACE e = expr RIGHT_BRACE { Ast.Func (ps, e) }
    | e = expr LEFT_PAREN args = separated_list(COMMA, colon_arg) RIGHT_PAREN { Ast.Call (e, args) }
    | i = IDENT EQUAL e = expr { Ast.Assign (i, e) }
    | i = IDENT { Ast.Ident i }
    | i = INTEGER { Ast.Integer i }
    | f = FLOAT { Ast.Float f }
    | s = STRING { Ast.String s }
    | d = DURATION { Ast.Duration d }
    | t = TIME { Ast.Time t }
    | r = REGEX { Ast.Regex r }
    | c = CHAR { Ast.Char c }
    | e1 = expr PLUS  e2 = expr { Ast.Plus (e1, e2) }
    | e1 = expr MINUS e2 = expr { Ast.Minus (e1, e2) }
    | e1 = expr TIMES e2 = expr { Ast.Times (e1, e2) }
    | e1 = expr DIV e2 = expr { Ast.Div (e1, e2) }
    | MINUS e = expr %prec UMINUS { Ast.Uminus e }
    | e1 = expr PIPE e2 = expr { Ast.Pipe (e1, e2) }
    | LEFT_BRACKET es = separated_list(COMMA, expr) RIGHT_BRACKET { Ast.List es }
    | LEFT_BRACE vs = separated_list(COMMA, colon_arg) RIGHT_BRACE { Ast.Record vs }
    | e = expr DOT i = IDENT { Ast.Select (e, i) }
    | e = expr LEFT_BRACKET i = expr RIGHT_BRACKET { Ast.Index (e, i) }
    | LEFT_PAREN e = expr RIGHT_PAREN { e }
    | e1 = expr c = COMP e2 = expr { Ast.Comp (e1, c, e2) }
    | e1 = expr AND e2 = expr { Ast.And (e1, e2) }
    | e1 = expr OR e2 = expr { Ast.Or (e1, e2) }
    | RETURN e = expr { Ast.Return e }
    ;

colon_arg:
    n = IDENT COLON e = expr { (n, e) };

func_param:
    n = IDENT
    d = func_param_default?
    { (n, d) }

func_param_default:
    | EQUAL PIPE_ARROW { Ast.DPipe }
    | EQUAL e = expr { Ast.DExpr e }
