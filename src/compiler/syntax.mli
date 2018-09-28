type variant_tag = string

type const = 
	| CInt of int
	| CFloat of float
	| CString of string
	| CChar of char

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
	| OGetTupleWithLen of int * int
	| Oiadd of int | Oibysub of int
	| Oimul of int | Oibydiv of int
	| OiArrRead of int | OiArrWrite of int

val op2str : optype -> string

type name = string 

type pattern = 
	| PVar     of name
	| PVariant    of variant_tag
	| PVariantApp of variant_tag * pattern
	| PTuple   of (pattern list)

type expr = expr_base * Debug.debug_data
and expr_base =
  | EConst of const
  | EVar       of name
  | EOp        of optype * (expr list)
  | EIf        of expr * expr * expr
  | ELet       of name * expr * expr
  | ELetRec    of name * (pattern list) * expr * expr
  | EApp       of expr * (expr list)
  | ETuple     of (expr list)
  | ELetTuple  of (name list) * expr * expr
  | EMatch     of expr * ((pattern * expr) list)
  | EVariant   of variant_tag * expr list

type type_expr = 
	| ETInt
	| ETFloat
	| ETVar     of name (* ふつうに、 int とか list とか *)
	| ETTyParam  of name (* 'a とかの、型多相のためのやつ *)
	| ETTuple   of type_expr list
	| ETTyFun   of (type_expr list) * type_expr (* 部分適用できないのでクリティカル *)
	| ETTyApp   of (type_expr list) * name


type decl = 
  | DLet        of name * expr
  | DLetRec     of name * (name list) * expr
  | DTypeRename of name * type_expr
  | DVariant    of name * ((variant_tag * (type_expr list)) list) 
  | DOpen       of name

type decl_expr = 
	| FExpr of expr
	| FDecl of decl

type top = decl_expr list

val top2str : top -> string


