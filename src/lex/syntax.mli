type cset =
	| CChar of string
	| CRange of string * string

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

type decllist = (idname * regexp) list

type code = string
type entries = (regexp * code) list

type entname = string
type argname = string
type rules = (entname * (argname list) * entries) list

type top = decllist * rules

