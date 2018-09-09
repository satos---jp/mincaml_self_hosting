open Syntax
open Debug
open Type_checker

type name = string * (ty * debug_data)

type kexp = 
	| KConst of Syntax.const
	| KOp        of Syntax.optype * (name list)
	| KIf        of comptype * name * name * kexp * kexp
	| KLet       of name * kexp * kexp
	| KVar       of name
	| KLetRec    of name * (name list) * kexp * kexp
	| KApp       of name * (name list)
	| KTuple     of (name list)
	| KLetTuple  of (name list) * name * kexp

val kexp_recconv : (kexp -> kexp) -> kexp -> kexp
val knorm2str : kexp -> string
val kexp_size : kexp -> int
val knorm : texp -> kexp

type hash = Vs of int * (string list) | C of const | N of hash list 

val hasher : kexp -> hash


