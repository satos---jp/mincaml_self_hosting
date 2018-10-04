%{
	open Syntax
	
%}

%token <string> ID
%token <string> HEADER
%token <string * int list> ACTION
%token <string> TYPEXPR
%token <int> TAG
%token BAR COLON SEMI
%token PCTOKEN PCSTART PCTYPE PCLEFT PCRIGHT PCNONASSOC PCPREC
%token PCPC EOF


%start toplevel 
%type <Syntax.top> toplevel

%% 

toplevel:
  | HEADER prec_exprs PCPC rule_exprs { ($1,$2,$4) }
  | prec_exprs PCPC rule_exprs { ("",$1,$3) }
;

prec_exprs: 
	| prec_expr { $1 } 
	| prec_exprs prec_expr { $1 @ $2 }
;

prec_expr:
	| PCTOKEN constr_list { List.fold_left (fun r x -> PToken(x) :: r) []  $2 }
	| PCTOKEN typexpr constr_list { List.fold_left (fun r x -> PTToken($2,x) :: r) [] $3 }
	| PCSTART symbol_list { List.fold_left (fun r x -> PStart(x) :: r) []  $2 }
	| PCTYPE typexpr symbol_list { List.fold_left (fun r x -> PType($2,x) :: r) []  $3 }
	| PCLEFT symbol_list { [PLeft($2)] }
	| PCRIGHT symbol_list { [PRight($2)] }
	| PCNONASSOC symbol_list { [PNonassoc($2)] }
;

rule_exprs: 
	| rule_expr { [$1] } 
	| rule_exprs rule_expr { $1 @ [$2] }
;

rule_expr: 
	| symbol COLON symbol_act_list SEMI { ($1,$3) } 
;

symbol_act_list:
	| symbol_act { [$1] }
	| BAR symbol_act { [$2] }
	| symbol_act_list BAR symbol_act { $1 @ [$3] }
;

symbol_act:
	| tag_symbol_list ACTION { ($1,$2) }
;

tag_symbol_list:
	| tag_symbol { [$1] }
	| tag_symbol_list tag_symbol { $1 @ [$2] }
;

tag_symbol:
	| symbol { $1 }
;

constr_list:
	| constr { [$1] }
	| constr_list constr { $1 @ [ $2 ] }

constr:
	| ID { $1 }
;

typexpr:
	| TYPEXPR { $1 }
;

symbol_list:
	| symbol { [$1] }
	| symbol_list symbol { $1 @ [ $2 ] }

symbol:
	| ID { $1 }
;
