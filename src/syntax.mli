type name = string 

type const = 
	| CInt of int
	| CFloat of float

val const2str : const -> string

type comptype = 
	| CmpEq
	| CmpLt

val comptype2str : comptype -> string

type optype = 
	| Ominus | Oadd | Osub | Omul | Odiv
	| Ofadd | Ofsub | Ofmul | Ofdiv
	| Oeq | Oneq | Olt | Oleq | Ogt | Ogeq | Osemi1 | Osemi2 | Onot 
	| OArrCrt | OArrRead | OArrWrite
	| OSubTuple of int * int
	| OGetTuple of int
	| Oiadd of int | Oibysub of int
	| Oimul of int | Oibydiv of int
	| OiArrRead of int | OiArrWrite of int

val op2str : optype -> string

type expr = expr_base * Debug.debug_data
and expr_base =
  | EConst of const
  | EVar       of name
  | EOp        of optype * (expr list)
  | EIf        of expr * expr * expr
  | ELet       of name * expr * expr
  | ELetRec    of name * (name list) * expr * expr
  | EApp       of expr * (expr list)
  | ETuple     of (expr list)
  | ELetTuple  of (name list) * expr * expr

type decl = 
  | DDecl      of (expr -> expr) list
  | DExpr      of expr

