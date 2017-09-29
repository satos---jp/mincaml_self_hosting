%{
  open Syntax
	let hoge = ref [0]
%}

%token <int>    INT
%token <bool>   BOOL 
%token <string> ID
%token <float>  FLOAT
%token LET IN				  
%token PLUS TIMES MINUS DIV
%token FPLUS FTIMES FMINUS FDIV
%token EQ LT LEQ GT GEQ NEQ
%token IF THEN ELSE NOT
%token LPAR RPAR 
%token FUN ARROW
%token REC
%token ALLCREATE
%token DOT COMMA SEMI EOF

%nonassoc IN
%right SEMI
%right if_assoc
%right arrow_assoc
%nonassoc tuple_assoc
%left COMMA
%left EQ LT LEQ GT GEQ NEQ
%left PLUS MINUS FPLUS FMINUS
%left TIMES DIV FTIMES FDIV
%left app_assoc  /* appの結合の強さはこれくらいで、の指示 */
%nonassoc NOT
%left DOT

%start toplevel 
%type <Syntax.expr> toplevel
%% 

toplevel:
  | expr EOF { $1 }
;


/* simple_expr のみが、関数適用に使える。 */

simple_expr:
	| LPAR expr RPAR
		{ $2 }
	| LPAR RPAR
		{ ETuple([]) }
	| BOOL
		{ EConst(CBool $1) }
	| INT 
		{ EConst(CInt $1) }
	| FLOAT
		{ EConst(CFloat $1) }
	| var
		{ EVar($1) }
	| simple_expr DOT LPAR expr RPAR
		{ EArrRead($1,$4) }
;

expr:
	| simple_expr
		{ $1 }
	| NOT expr
		{ EOp("not",[$2]) }
/* 
# -(5.4);;
- : float = -5.4
# -(5.4 +. 9.0);;
Error: This expression has type float but an expression was expected of type
         int
なので、これの伝搬をしておく。
*/
	| MINUS expr
		{ match $2 with
			| EConst(CFloat(f)) -> EConst(CFloat(-.f))
			| _ -> EOp("minus",[$2]) }
	| expr PLUS expr              
		{ EOp("add",[$1;$3]) }
	| expr MINUS expr 
		{ EOp("sub",[$1;$3]) }
	| expr TIMES expr 
		{ EOp("mul",[$1;$3]) }
	| expr DIV expr 
		{ EOp("div",[$1;$3]) }
	| expr FPLUS expr              
		{ EOp("fadd",[$1;$3]) }
	| expr FMINUS expr 
		{ EOp("fsub",[$1;$3]) }
	| expr FTIMES expr 
		{ EOp("fmul",[$1;$3]) }
	| expr FDIV expr 
		{ EOp("fdiv",[$1;$3]) }
	| expr EQ expr 
		{ EOp("eq",[$1;$3]) }
	| expr NEQ expr 
		{ EOp("neq",[$1;$3]) }
	| expr LT expr 
		{ EOp("lt",[$1;$3]) }
	| expr LEQ expr 
		{ EOp("leq",[$1;$3]) }
	| expr GT expr 
		{ EOp("gt",[$1;$3]) }
	| expr GEQ expr 
		{ EOp("geq",[$1;$3]) }
	| expr SEMI
		{ EOp("semi",[$1]) }
	| expr SEMI expr 
		{ EOp("semi",[$1;$3]) }
/*
セミコロン1つだけもvalidらしい(本家パーザにはないが)
*/
	| IF expr THEN expr ELSE expr
		%prec if_assoc
		{ EIf($2,$4,$6) }
	| LET var EQ expr IN expr
		{ ELet($2,$4,$6) }
	| LET REC rec_vars EQ expr IN expr { ELetRec(List.hd $3,List.tl $3,$5,$7) }
	| simple_expr app_exprs
		%prec app_assoc
		{ List.fold_left 
			(fun r -> fun a -> EApp(r,a)) $1 $2 }
	| tuple_exprs                 
		%prec tuple_assoc
		{ ETuple($1)  }
	| LET LPAR tuple_vars EQ expr IN expr 
		{ ELetTuple($3,$5,$7) }
	| ALLCREATE simple_expr simple_expr             { EArrCrt($2,$3) }
	| simple_expr DOT LPAR expr RPAR ARROW expr 
		%prec arrow_assoc
		{ EArrWrite($1,$4,$7) }  
	| error
    { Syntax.err := ((Parsing.symbol_start ()), (Parsing.symbol_end ()));
    	failwith "mly error" }
/*
    { failwith 
    	
        (Printf.sprintf "parse error near characters %d-%d"
           (Parsing.symbol_start ())
           (Parsing.symbol_end ())) }
*/
;

tuple_exprs:
	| tuple_exprs COMMA expr{ $1 @ [$3] }
	| expr COMMA expr { [$1; $3] }
;


app_exprs:
	| simple_expr app_exprs
		{ $1 :: $2 }
	| simple_expr 
		{ [$1] }
;


tuple_vars:
	| RPAR { [] }
	| var RPAR { [$1] }
	| var COMMA tuple_vars { $1 :: $3 }
;

rec_vars:
	| var rec_vars { $1 :: $2 }
	| var { [$1] } 
;

var:
  | ID { $1 }
;
