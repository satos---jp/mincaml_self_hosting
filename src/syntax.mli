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

type variant_tag = string

type pattern = 
	| PVar     of name
	| PVariant of name * (pattern list)

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
  | EMatch     of expr * ((pattern * expr) list)
  
val expr2str : expr -> string

type type_expr = 
	| ETInt
	| ETFloat
	| ETTuple   of type_expr list
	| ETyFun    of type_expr * type_expr

type decl = 
  | DLet        of name * expr
  | DLetRec     of name * (name list) * expr
  | DTypeRename of name * type_expr
  | DVariant    of name * ((variant_tag * type_expr) list)

type decl_expr = 
	| FExpr of expr
	| FDecl of decl

type top = decl_expr list

val top2str : top -> string


