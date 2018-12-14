%{
	open Syntax
	
	let cstr2char s = 
		let c1 = String.get s 0 in
		if c1 = '\\' then (
			if String.length s <> 2 then failwith (Printf.sprintf "Unknown escape sequence '%s'" s) else
			let c2 = String.get s 1 in  (* このへんがコンパイラととともに受け継がれていく情報 *)
			if c2 = 'n' then '\n'
			else if c2 = 't' then '\t'
			else if c2 = 'r' then '\r'
			else if c2 = '\'' then '\''
			else if c2 = '\\' then '\\'
			else failwith (Printf.sprintf "Unknown escape sequence '%s'" s)
		) else (
			if String.length s = 1 then c1 else failwith (Printf.sprintf "Invalid char '%s'" s)
		)
	
	let rec string_unescape s = 
		let ls = String.length s in
		if ls = 0 then ""
		else (
			let c1 = String.get s 0 in
			if c1 = '\\' then (
				(String.make 1 (cstr2char (String.sub s 0 2))) ^ (string_unescape (String.sub s 2 (ls-2)))
			)
			else (
				(String.sub s 0 1) ^ (string_unescape (String.sub s 1 (ls-1)))
			)
		)
%}

%token <string> CHAR
%token <string> STRING
%token <string> CODE
%token <string> ID
%token LET EQ RULE PARSE AND AS
%token BAR LPAR RPAR LBRA RBRA 
%token STAR MINUS UNSCO PLUS
%token EOF

/* %left BAR なくても大丈夫ぽいですね？ */

%start toplevel 
%type <Syntax.top> toplevel

%% 

toplevel:
  | let_exprs rule_exprs EOF { ($1,$2) }
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
	| STRING { RStr(string_unescape $1) }
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
	| RULE               { initial_env () }
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


