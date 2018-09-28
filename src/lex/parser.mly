%{
	open Syntax
	
	let cstr2char s = 
		assert (String.length s = 1);
		String.get s 1
%}

%token <string> CHAR
%token <string> STRING
%token <string> CODE
%token <string> ID
%token LET EQ RULE PARSE AND AS
%token BAR LPAR RPAR LBRA RBRA 
%token STAR MINUS UNSCO PLUS
%token EOF

%left BAR

%start toplevel 
%type <Syntax.top> toplevel

%% 

toplevel:
  | let_exprs rule_exprs { ($1,$2) }
;

cset_expr:
	| CHAR { CChar(cstr2char $1) }
	| CHAR MINUS CHAR { CRange(cstr2char $1,cstr2char $3) }
;
	
cset_exprs:
	| cset_expr { [$1] }
	| cset_expr cset_exprs { $1 :: $2 }
;

simple_regexp:
	| CHAR { RCset([CChar(cstr2char $1)]) }
	| STRING { RStr($1) }
	| ID     { RId($1) }
	| LBRA cset_exprs RBRA { RCset($2) }
	| LPAR regexp_expr RPAR { $2 }
	| UNSCO { RAll }
;

star_plus_regexp:
	| simple_regexp { $1 }
	| simple_regexp STAR { RStar($1) }
	| simple_regexp PLUS { RPlus($1) }
;


regexp_cons_list:
	| star_plus_regexp { [$1] }
	| regexp_cons_list star_plus_regexp { $1 @ [$2] }
; 

regexp_or_list:
	| regexp_cons_list { [RCons($1)] }
	| regexp_or_list BAR regexp_cons_list { $1 @ [RCons($3)] }
;


regexp_expr:
	| regexp_expr AS ID { RAs($1,$3) }
	| regexp_or_list { ROr($1) }
;

let_expr:
	| LET ID EQ regexp_expr { ($2,$4) }
;

let_exprs:
	| let_expr let_exprs { $1 :: $2 }
	| RULE               { []       }
;

entry_exprs:
	| BAR regexp_expr CODE { [($2,$3)] }
	| BAR regexp_expr CODE entry_exprs { ($2,$3) :: $4 }
;

args_eq:
	| ID args_eq { $1 :: $2 }
	| EQ         { [] }
;

rule_exprs:
	| ID args_eq PARSE entry_exprs { [($1,$2,$4)] }
	| ID args_eq PARSE entry_exprs AND rule_exprs { ($1,$2,$4) :: $6 }
;


