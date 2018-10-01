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
val tyvar2str : tyvar -> string
val gentype : unit -> ty


type tyscheme
val instanciate : tyscheme -> ty
val schemize : ty -> (string * tyscheme) list -> (tyvar * ty) list -> tyscheme
val no_fv_scheme : ty -> tyscheme
val tyscheme2str : tyscheme -> string
val ty_scheme_subst : (tyvar * ty) list -> tyscheme -> tyscheme


val ty_subst : (tyvar * ty) list -> ty -> ty
val unify : (type_name * ty) list -> (ty * ty * Debug.debug_data) list -> (tyvar * ty) list
exception TypeError of ty * ty * Debug.debug_data

val is_subtype : ty -> ty -> bool

