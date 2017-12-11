open Op
open Closure_conv

type funbody = VirtFunBody of op list * name list
type virtglobdef = VirtFunDef of name * (name list) * (name list) * funbody

val to_virtual : (name * globdef) list * (string list) * cexp -> virtglobdef list * funbody * string list

val virt2str : virtglobdef list * funbody * string list -> string
