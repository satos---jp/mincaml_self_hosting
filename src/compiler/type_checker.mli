open Syntax
open Spec
open Debug

type tyvar = int

type type_name = string

type ty =
	| TyInt
	| TyFloat
	| TyStr
	| TyChar
	| TyNum  (* int も float もこれの部分型 *)
	| TyVar of tyvar
	| TyArr of ty
	| TyFun of (ty list) * ty
	| TyTuple of ty list
	| TyUserDef of type_name * (ty list)

val type2str : ty -> string

type name = string * (ty * debug_data)

type texp = texp_base * (ty * debug_data)
and texp_base =
  | TConst of const
  | TVar       of name
  | TOp        of optype * (texp list)
  | TIf        of texp * texp * texp
  | TLet       of name * texp * texp
  | TLetRec    of name * (name list) * texp * texp
  | TApp       of texp * (texp list)
  | TTuple     of (texp list)
  | TLetTuple  of (name list) * texp * texp

val global_funcs : unit -> string list

val get_exports  : unit -> string list
val get_imports  : unit -> (string * ty) list

val texp2str : texp -> string

(* 型情報は消していいので、これ以降はtexpのままでいいはず。 *)
val check : Syntax.top -> Spec.top -> texp


