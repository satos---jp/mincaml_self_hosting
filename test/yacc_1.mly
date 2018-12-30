%token <int>    INT
%token ADD SUB MUL DIV 
%token LPAR RPAR 
%token NL

%start toplevel 
%type <int> toplevel
%% 

toplevel:
  | expr NL { $1 }
;

expr:
  | expr ADD factor_expr { $1 + $3 }
  | expr SUB factor_expr { $1 - $3 }
  | factor_expr { $1 }
;

factor_expr:
  | factor_expr MUL base_expr { $1 * $3 }
  | factor_expr DIV base_expr { $1 / $3 }
  | base_expr { $1 }
;

base_expr: 
  | INT            { $1 }
  | LPAR expr RPAR { $2 }
;

