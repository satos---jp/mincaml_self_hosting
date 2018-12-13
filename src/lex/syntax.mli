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

val initial_env : unit -> decllist
