%{
	open Syntax
	open Debug
	open Genint
	
	let debug expr = (expr , (get_debug_data ()))
	let gen_fun_name () = Printf.sprintf "@lam_%d" (genint ())
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
%token FUN LARROW RARROW
%token REC
%token ALLCREATE
%token DOT COMMA SEMI EOF
%token SEMISEMI TYPE BAR OF
%token <string> VARIANT_ID

/* 下のほうが強い */
/* left とか right とかは演算子のみに効果があるっぽいな？？？ */
%left IN
%right SEMI
%nonassoc LET
%right RARROW
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
%nonassoc unary_minus

%start toplevel 
%type <Syntax.top> toplevel
%% 

toplevel:
  | expr decl_exprs { FExpr($1) :: $2 }
	| decl decl_exprs { FDecl($1) :: $2 }
;

decl_exprs:
	| EOF { [] }
	| SEMISEMI expr decl_exprs { FExpr($2) :: $3 }
	| decl decl_exprs { FDecl($1) :: $2 }
;

decl:
	| LET var EQ expr           { DLet($2,$4) }
	/* これ多分parse時点で let rec f x1 x2 ... =  を let f = (let rec f x1 x2 ... in f) にしたほうが楽*/
	| LET REC rec_vars EQ expr { 
			(* DLetRec(List.hd $3,List.tl $3,$5) *) 
			let na = List.hd $3 in
			let vs = List.tl $3 in
			DLet(na,debug (ELetRec(na,vs,$5,(debug (EVar(na))))))
		}
	| TYPE var EQ type_expr    { DTypeRename($2,$4) }
	| TYPE var EQ variant_defs { DVariant($2,$4) }
;

variant_defs:
	| variant_def                      { [$1] }
	|     variant_def BAR variant_defs { $1 :: $3 }
	| BAR variant_def BAR variant_defs { $2 :: $4 }
;

variant_def:
	| variant_name OF type_expr { ($1,$3) }
;

variant_name:
	| VARIANT_ID { $1 }
;

tuple_type_exprs:
	| tuple_type_exprs TIMES type_expr 
		%prec tuple_assoc    
		{ $1 @ [$3] }
	| type_expr TIMES type_expr	
		%prec tuple_assoc    
		{ [$1; $3] }
;

type_expr:
	| INT                         { ETInt   }
	| FLOAT                       { ETFloat   }
	| tuple_type_exprs
		%prec tuple_assoc            
		{ ETTuple($1) }
	| type_expr RARROW type_expr  { ETyFun($1,$3) }
	| LPAR type_expr RPAR { $2 }

/* simple_expr のみが、関数適用に使える。 */

simple_expr:
	| LPAR expr RPAR
		{ $2 }
	| LPAR RPAR
		{ debug (ETuple([])) }
	| INT 
		{ debug (EConst(CInt $1)) }
	| FLOAT
		{ debug (EConst(CFloat $1)) }
	| var
		{ debug (EVar($1)) }
	| simple_expr DOT LPAR expr RPAR
		{ debug (EOp(OArrRead,[$1;$4])) }
;

expr:
	| simple_expr
		{ $1 }
	| NOT expr
		{ debug (EOp(Onot,[$2])) }
/* 
# -(5.4);;
- : float = -5.4
# -(5.4 +. 9.0);;
Error: This expression has type float but an expression was expected of type
         int
なので、これの伝搬をしておく。

また、結合強さはもとの-のままではだめで、 - 1.2 * 3.4 とかが -(1.2*3.4)とかになってしまう
*/
	| MINUS expr
		%prec unary_minus
		{ match $2 with
			| EConst(CFloat(f)),d2 -> EConst(CFloat(-.f)),d2
			| _ ->  debug (EOp(Ominus,[$2])) }
	| expr PLUS expr              
		{ debug (EOp(Oadd,[$1;$3])) }
	| expr MINUS expr 
		{ debug (EOp(Osub,[$1;$3])) }
	| expr TIMES expr 
		{ debug (EOp(Omul,[$1;$3])) }
	| expr DIV expr 
		{ debug (EOp(Odiv,[$1;$3])) }
	| expr FPLUS expr              
		{ debug (EOp(Ofadd,[$1;$3])) }
	| expr FMINUS expr 
		{ debug (EOp(Ofsub,[$1;$3])) }
	| expr FTIMES expr 
		{ debug (EOp(Ofmul,[$1;$3])) }
	| expr FDIV expr 
		{ debug (EOp(Ofdiv,[$1;$3])) }
	| expr EQ expr 
		{ debug (EOp(Oeq,[$1;$3])) }
	| expr NEQ expr 
		{ debug (EOp(Oneq,[$1;$3])) }
	| expr LT expr 
		{ debug (EOp(Olt,[$1;$3])) }
	| expr LEQ expr 
		{ debug (EOp(Oleq,[$1;$3])) }
	| expr GT expr 
		{ debug (EOp(Ogt,[$1;$3])) }
	| expr GEQ expr 
		{ debug (EOp(Ogeq,[$1;$3])) }
	| expr SEMI
		{ debug (EOp(Osemi1,[$1])) }
	| expr SEMI expr 
		{ debug (EOp(Osemi2,[$1;$3])) }
/*
セミコロン1つだけもvalidらしい(本家パーザにはないが)
*/
	| IF expr THEN expr ELSE expr
		%prec if_assoc
		{ debug (EIf($2,$4,$6)) }
	| LET var EQ expr IN expr
		{ debug (ELet($2,$4,$6)) }
	| LET REC rec_vars EQ expr IN expr 
		{  debug (ELetRec(List.hd $3,List.tl $3,$5,$7)) }
/* カリー化できないらしい!! (まあMinCamlプログラミングがつらくなる) */
	| simple_expr app_exprs
		%prec app_assoc
		{ debug (EApp($1,$2)) }
	| tuple_exprs                 
		%prec tuple_assoc
		{ debug (ETuple($1))  }
	| LET LPAR tuple_vars EQ expr IN expr 
		{ debug (ELetTuple($3,$5,$7)) }
	| ALLCREATE simple_expr simple_expr             
		{ debug (EOp(OArrCrt,[$2;$3])) }
	| simple_expr DOT LPAR expr RPAR LARROW expr 
		%prec arrow_assoc
		{ debug (EOp(OArrWrite,[$1;$4;$7])) }  
		
	| FUN rec_vars RARROW expr
		{ let fn = gen_fun_name () in
			debug (ELetRec(fn,$2,$4,debug (EVar(fn)))) }
	| error
		{ failwith ("parse failure at " ^ debug_data2str (get_debug_data ())) }
;

tuple_exprs:
	| tuple_exprs COMMA expr { $1 @ [$3] }
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
