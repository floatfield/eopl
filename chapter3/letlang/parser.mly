%token <float> FLOAT
%token <string> ID
%token EOF LPAREN RPAREN MINUS ZERO LET EQ IN COMMA DASH
%token EMPTYLIST CONS CAR CDR ISNULL LIST
%token IF THEN ELSE

%start <Ast.expr option> prog

%%

prog:
    | e = expr; EOF { Some e }
    | EOF { None }
    ;

expr:
    | x = FLOAT { Ast.ConstExp x }
    | MINUS; LPAREN; e = expr; RPAREN; { Ast.MinusExp e }
    | DASH; LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN; { Ast.DiffExp (e1,e2) }
    | ZERO; LPAREN; e = expr; RPAREN; { Ast.IsZeroExp e }
    | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr; { Ast.IfExp (e1,e2,e3) }
    | v = ID; { Ast.VarExp v }
    | LET v = ID; EQ; e1 = expr; IN; e2 = expr; { Ast.LetExp (v,e1,e2) }
    | EMPTYLIST { Ast.EmptyListExp }
    | CONS; LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN; { Ast.ConsExp (e1,e2) }
    | CAR; LPAREN; e = expr; RPAREN; { Ast.CarExp e}
    | CDR; LPAREN; e = expr; RPAREN; { Ast.CdrExp e}
    | ISNULL; LPAREN; e = expr; RPAREN; { Ast.IsNullExp e}
    | LIST; LPAREN; es = separated_list(COMMA, expr); RPAREN; { Ast.ListOf (List.rev es) }
    ;