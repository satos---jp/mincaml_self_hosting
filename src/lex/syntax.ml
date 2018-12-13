type cset =
	| CChar of char
	| CRange of char * char

type idname = string

type regexp = 
	| RCset  of cset list
	| ROr    of regexp list
	| RStar  of regexp
	| RPlus  of regexp
	| RId    of idname
	| RStr   of string
	| RCons  of regexp list
	| RAs    of regexp * string
	| RAll
	| REof

type decllist = (idname * regexp) list

type code = string
type entries = (regexp * code) list

type entname = string
type argname = string
type rules = (entname * (argname list) * entries) list

type top = decllist * rules

let initial_env _ = 
	[("eof",REof)]

(*
Tes.main (Lexing.from_string "hogey");;
これが動くように。

最終的には

Parser.toplevel Lexer.main (Lexing.from_channel ic)

なので、 Parser.toplevel の型をいい感じに改変して、
あー、内部挙動の関係で、やっぱり lexbuf がmutableでないとだめぽ。



open Lexing

val main : arg1 * arg2 * lexbuf -> rettype 的な。


let rec main v1 ... vn = 
	

*)
