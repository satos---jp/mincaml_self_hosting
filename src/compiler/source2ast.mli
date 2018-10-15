open Syntax
open Spec
val src2ast : string -> (Syntax.top * (string list))
val open2spec : string -> Spec.top
val basename : string -> string
