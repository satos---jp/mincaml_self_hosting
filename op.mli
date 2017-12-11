open Syntax
open Type_checker
open Debug


(*
type namestr = Var of string | Reg of string
*)
type name = string * (ty * debug_data)

val name2str : name -> string
val vs2str : name list -> string

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

val virtop2str : op -> string
