open Syntax
open Spec
val src2ast : string -> Syntax.top
val open2spec : string -> Spec.top
val basename : string -> string
val stdlib_list : string list
