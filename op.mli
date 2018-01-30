open Syntax
open Type_checker
open Debug



(* グローバル変数と関数ラベル名 | ローカル変数 | レジスタ名 *)
type namestr = GVar of string | Var of string | Reg of string
type namereg = (namestr ref) * (ty * debug_data)

val namestr2str : namestr -> string 
val namereg2str : namereg -> string
val vs2str : namereg list -> string

type name = string * (ty * debug_data)


type label = string
type istailcall = Tail | NonTail
type isdirapp   = DirApp | InDirApp
type op = 
	| OpMovi  of namereg * const
	| OpMov   of namereg * namereg
	| OpOpr   of namereg * optype * (namereg list)
	| OpJcnd  of comptype * namereg * namereg * label
	| OpLabel of label 
	| OpJmp   of label 
	| OpApp   of istailcall * isdirapp * namereg * namereg * (namereg list)
	| OpMakeTuple of namereg * (namereg list)
	| OpDestTuple of (namereg list) * namereg
	| OpMakeCls   of namereg * name * (namereg list)
	| OpRet       of namereg
	| OpMainRet


val virtop2str : op -> string

val get_var_nameregs : op -> namereg list
val get_assigned : op -> namestr list
val get_assigner : op -> (namestr ref) list

val remove_useless_jump : op list -> op list





