%{
	open Syntax
	open Debug
	open Genint
	open Spec
	
	let debug expr = (expr , (get_debug_data ()))
	let gen_fun_name () = Printf.sprintf "@lam_%d" (genint ())
%}

%token <int>    INT
%token <bool>   BOOL 
%token <string> ID
%token <float>  FLOAT
%token <string> STRING
%token <char>   CHAR
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
%token SEMISEMI TYPE BAR OF MATCH WITH
%token <string> VARIANT_ID
%token CONS LBRACKET RBRACKET
%token VAL COLON OPEN QUOTE
%token ATMARK CARET FLAG
%token COLONEQ EXCLAMATION 
%token AND
 

%nonassoc only_variant_name_simple_expr prec_tytuple_times_expr simple_variant_name_pattern type_expr_prec tyfun_arrow_expr_prec 

/* TODO(satos) VARIANT_ID の優先度をどうにかする (そのまま付け加えると A B みたいのがだめになる)*/

/* これらは、先読みできるなら待って読んだほうがいいやつ。 */

%left IN
%nonassoc fun_assoc
%right SEMI
%nonassoc LET
%left RARROW
%right if_assoc
%right arrow_assoc
%right COLONEQ
%nonassoc list_semi_assoc
%nonassoc tuple_assoc
%nonassoc variant_tuple_assoc
%nonassoc variant_apply_pattern
%left COMMA
%left EQ LT LEQ GT GEQ NEQ BAR
%right ATMARK CARET
%left FLAG /* TODO(satos) ちゃんと調べて直す */
%right CONS
%left PLUS MINUS FPLUS FMINUS
%left TIMES DIV FTIMES FDIV 
%nonassoc EXCLAMATION
%nonassoc variant_def_tuple_type_exprs_assoc /* TIMESより強くないといけない */
%left app_assoc  /* appの結合の強さはこれくらいで、の指示 */
%nonassoc variant_app_assoc
%nonassoc NOT
%left DOT
%nonassoc array_assoc
%nonassoc unary_minus
%nonassoc path_name_prec

/* これらは、待たないやつ(simple_exprなので強めに結合する) */
%nonassoc INT ID FLOAT STRING CHAR LPAR RPAR LBRACKET LBRACKET 


%start toplevel 
%type <Syntax.top> toplevel

%start specification_list
%type <Spec.top> specification_list

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


let_rec_body:
	| ID rec_vars EQ expr { $1,$2,$4 }
;

let_rec_and_list:
	| LET REC let_rec_body { [$3] }
	| let_rec_and_list AND let_rec_body { $1 @ [$3] }
;

decl:
	| LET ID EQ expr           { DLet($2,$4) }
	| let_rec_and_list         { DLetRec($1) }
	| LET ID rec_vars EQ expr { 
			(* とりあえず意味は壊れるがやっていく *) 
			DLetRec([$2,$3,$5])
		}
	| TYPE var EQ type_expr    { DTypeRename($2,$4) }
	| TYPE var EQ variant_defs { DVariant($2,$4) }
	| OPEN variant_name        { implicit_open $2; DOpen($2) }
;


specification_list:
	| specification  specification_list { $1 :: $2 }
	| EOF    { [] }
;

specification:
	| VAL var COLON type_expr  { SValtype($2,$4)    }
	| TYPE var EQ type_expr    { STypeRename($2,$4) }
	| TYPE var EQ variant_defs { SVariant($2,$4) }
	| OPEN variant_name        { implicit_open $2; SOpen($2) }
;


variant_defs:
	| variant_def                      { [$1] }
	| BAR variant_def                  { [$2] }
	|     variant_def BAR variant_defs { $1 :: $3 }
	| BAR variant_def BAR variant_defs { $2 :: $4 }
;


constr_args:
	| type_expr
		%prec variant_def_tuple_type_exprs_assoc
		{ [$1] }
	| constr_args TIMES type_expr
		%prec variant_def_tuple_type_exprs_assoc
		{ $1 @ [$3] }
;

variant_def:
	| variant_name              { ($1,[]) }
	| variant_name OF constr_args { ($1,$3) }
;



/*
	int list * int list -> int list は
	((int list) * (int list)) -> int list
	みたいになる。
	
	tyfun_expr
	tytuple_expr
	tyapp_expr
	tybase_expr
	
	で。
	#	–
	*	–
	->	right
	as	– 
*/

/*
typexpr	::=	' ident  
 	∣	 _  
 	∣	 typeconstr  
 	∣	 ( typexpr )  
 	∣	 [[?]label-name:]  typexpr ->  typexpr  
 	∣	 typexpr  { * typexpr }+  
 	∣	 typexpr  typeconstr  
 	∣	 ( typexpr  { , typexpr } )  typeconstr 
 
 をサポートする。
*/

/* TODO(satos) このへんの名前expr_varとか直す */


tyarg_expr:
	| type_expr            
		%prec type_expr_prec
		{ [$1] }
	| tyarg_expr COMMA type_expr { $1 @ [$3] }
;

tytuple_times_expr:
	| tytuple_times_expr TIMES type_expr 	{ $1 @ [$3] }
	| type_expr TIMES type_expr	    { [$1; $3] }
;

/* 部分的用できないので、ここ分けないとクリティカルに効いてきてしまう */
tyfun_arrow_expr:
	| tyfun_arrow_expr RARROW type_expr { let a,b = $1 in (a @ [b],$3) }
	| type_expr RARROW type_expr { ([$1],$3) }
;

/* applyとかできるのはこれだけ、にするやつ。 */
simple_type_expr:
 	| expr_var                         { ETVar($1) }
 	| QUOTE expr_var                   { ETTyParam($2) }
	| LPAR type_expr RPAR              { $2 }
;

type_expr:
	| simple_type_expr                 { $1 }
	| tytuple_times_expr               
		%prec prec_tytuple_times_expr
		{ ETTuple($1) }
	| tyfun_arrow_expr                 
		%prec tyfun_arrow_expr_prec
		{ let a,b = $1 in ETFun(a,b) }
	| type_expr var                    { ETApp([$1],$2) }
	| LPAR tyarg_expr RPAR var         { ETApp($2,$4) }
;

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
	| STRING
		{ debug (EConst(CString (Scanf.unescaped $1))) }
	| CHAR 
		{ debug (EConst(CChar $1)) }
	| expr_var
		{ debug (EVar($1)) }
	| simple_expr DOT LPAR expr RPAR
		%prec array_assoc
		{ debug (EOp(OArrRead,[$1;$4])) }
	| variant_name
		%prec only_variant_name_simple_expr
		{ debug (EVariant($1,[])) }
	| LBRACKET RBRACKET 
		{ debug (EVariant("@Nil",[])) }
	| LBRACKET list_expr RBRACKET 
		{ List.fold_left (fun r x -> debug (EVariant("@Cons",[x;r]))) (debug (EVariant("@Nil",[]))) $2 }
	| LBRACKET list_expr SEMI RBRACKET 
		{ List.fold_left (fun r x -> debug (EVariant("@Cons",[x;r]))) (debug (EVariant("@Nil",[]))) $2 }
;

/*
# type a = H of int;;
type a = H of int
# let x = H;;
Error: The constructor H expects 1 argument(s),
       but is applied here to 0 argument(s)

こんな感じなので、 variant_name  は(あれば)適用と合わせてsimpl_expr に入れる必要がありそう。

というか、 EConstではなくて EVariant あたりで 適用状態でパースされるべきっぽい。

# f H(3);;
Error: The constructor H expects 1 argument(s),
       but is applied here to 0 argument(s)

フムー
*/

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
	| LET ID EQ expr IN expr
		{ debug (ELet($2,$4,$6)) }
	| LET REC ID rec_vars EQ expr IN expr 
		{  debug (ELetRec([$3,$4,$6],$8)) }
/* カリー化できないらしい!! (まあMinCamlプログラミングがつらくなる) */
	| LET ID rec_vars EQ expr IN expr 
		{  debug (ELetRec([$2,$3,$5],$7)) }
/* とりあえず増やす(型推論がガバガバだが) */
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
		%prec fun_assoc
		{ let fn = gen_fun_name () in
			debug (ELetRec([fn,$2,$4],debug (EVar(fn)))) }
	| MATCH expr WITH cases
		{ debug (EMatch($2,$4)) }
	| variant_expr 
		{ debug $1 }
	| expr CONS expr
		{ debug (EVariant("@Cons",[$1;$3])) }

	| expr ATMARK expr
		{ implicit_open "List";
			debug (EApp(debug (EVar("List@append")),[$1;$3])) }
	| expr CARET expr
		{ implicit_open "String";
			debug (EApp(debug (EVar("String@@")),[$1;$3])) }
	| expr FLAG expr
		{ debug (EApp($3,[$1])) }
	| expr COLONEQ expr
		{ debug (EApp(debug (EVar("@ref@set")),[$1;$3])) }
	| EXCLAMATION expr
		{ debug (EApp(debug (EVar("@ref@get")),[$2])) }
	| error
		{ failwith ("parse failure at " ^ debug_data2str (get_debug_data ())) }
;

list_expr:
	| expr 
		%prec list_semi_assoc
		{ [$1] }
	| list_expr SEMI expr 
		%prec list_semi_assoc
		{ $3 :: $1 }
;



/*
	variant_name LPAR expr COMMA expr RPAR　が 
	variant_name LPAR tuple_expr RPAR　なのか
	variant_name simple_expr なのか(上にしてほしーが)
	
	さすがにそろそろ動かしたい
*/


variant_expr:
	/* TODO このconflictどうにかして解消する */
/*
	| variant_name             { EVariant($1,[]) }
	これはsimpl_expr 側に入る
*/
	| variant_name simple_expr
		%prec variant_app_assoc
		{ EVariant($1,[$2]) }
	| variant_name LPAR tuple_exprs RPAR 
		%prec variant_tuple_assoc
		{ EVariant($1,$3) }
;

cases:
	|     bar_patterns RARROW expr { [($1,$3)] }
	| BAR bar_patterns RARROW expr { [($2,$4)] }
	|     bar_patterns RARROW expr BAR cases { ($1,$3) :: $5 }
	| BAR bar_patterns RARROW expr BAR cases { ($2,$4) :: $6 }
;

bar_patterns:
	| pattern { [$1] }
	| bar_patterns BAR pattern { $1 @ [$3] }
;

/* TODO { as , _ , list , 複数match } に対応 */

tuple_patterns:
	| tuple_patterns COMMA pattern { $1 @ [$3] }
	| pattern COMMA pattern { [$1; $3] }
;

/*
# type a = H of int * int;;
type a = H of int * int
# fun x -> match x with H p,q -> p+q;;
Error: The constructor H expects 2 argument(s),
       but is applied here to 1 argument(s)
こんなん。

こーいうのもあるよなぁ...?



# type a = H of int * int;;
type a = H of int * int
# fun x -> match x with H(a,b) -> a + b;;
- : a -> int = <fun>
# fun x -> match x with H((a,b)) -> a + b;;
- : a -> int = <fun>
# H((1,2));;
- : a = H (1, 2)
# let y = 3,4;;
val y : int * int = (3, 4)
# H(y)::
  ;;
Error: Syntax error
# H(y);;
Error: The constructor H expects 2 argument(s),
       but is applied here to 1 argument(s)
# H y;;
Error: The constructor H expects 2 argument(s),
       but is applied here to 1 argument(s)

ま！？つってる
*/


list_pattern:
	| pattern
		%prec list_semi_assoc
		{ [$1] }
	| list_pattern SEMI pattern
		%prec list_semi_assoc
		{ $3 :: $1 }
;

/*
# fun p -> match p with A A x -> x;;
- : 'a x x -> 'a = <fun>
くらいの結合らしい。
*/

pattern:
	| var                  { PVar($1) }
	| LPAR pattern RPAR    { $2 }
	| variant_name         	
		%prec simple_variant_name_pattern
		{ PVariant($1) }
	| variant_name pattern
		%prec variant_apply_pattern
		{ PVariantApp($1,$2) }
	| tuple_patterns
		%prec tuple_assoc
		{ PTuple($1) }
	| pattern CONS pattern { PVariantApp("@Cons",PTuple([$1;$3])) }
	| LBRACKET RBRACKET { PVariant("@Nil") }
	| LBRACKET list_pattern RBRACKET { List.fold_left (fun r x -> (PVariantApp("@Cons",PTuple([x;r])))) (PVariant("@Nil")) $2 }
;

tuple_exprs:
	| tuple_exprs COMMA expr { $1 @ [$3] }
	| expr COMMA expr { [$1; $3] }
;

app_exprs:
	| simple_expr app_exprs { $1 :: $2 }
	| simple_expr { [$1] }
;


tuple_vars:
	| RPAR { [] }
	| expr_var RPAR { [$1] }
	| expr_var COMMA tuple_vars { $1 :: $3 }
;

rec_vars:
	| rec_vars pattern { $1 @ [$2] }
	| pattern { [$1] } 
;

expr_var:
	| var { $1 }
 	| VARIANT_ID DOT ID 
  	%prec path_name_prec
  	{ implicit_open $1;
  		$1 ^ "@" ^ $3 (* とりあえずこんな処理で(capitalizeされてるので) *) } 
;

variant_name:
	| VARIANT_ID { $1 }
	| VARIANT_ID DOT VARIANT_ID 
		{ implicit_open $1;
			$1 ^ "@" ^ $3 (* とりあえずこんな処理で(capitalizeされてるので) *) } 
;

var:
  | ID { $1 }
;

