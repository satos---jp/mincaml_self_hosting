open Virtual
open Type_checker

val vir2asm : virtglobdef list * funbody * string list -> (string * ty) list -> string list -> string

