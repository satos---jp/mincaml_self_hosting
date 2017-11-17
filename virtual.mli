open Syntax
open Type_checker
open Debug
open Closure_conv

type name = string * (ty * debug_data)

type label = string
type istailcall = Tail | NonTail
type isdirapp   = DirApp | InDirApp
type op = 
	| OpMovi  of name * const
	| OpMov   of name * name
	| OpOpr   of name * optype * (name list)
	| OpJcnd  of comptype * name * name * label
	| OpLabel of label 
	| OpJmp   of label 
	| OpApp   of istailcall * isdirapp * name * name * (name list)
	| OpMakeTuple of name * (name list)
	| OpDestTuple of (name list) * name
	| OpMakeCls   of name * name * (name list)
	| OpRet       of name
	| OpMainRet

type funbody = VirtFunBody of op list * name list
type virtglobdef = VirtFunDef of name * (name list) * (name list) * funbody

val to_virtual : (name * globdef) list * (string list) * cexp -> virtglobdef list * funbody * string list

