open Spec
open Type
open Type_expr

val global_funcs : unit -> string list

val get_exports  : unit -> string list
val get_imports  : unit -> (string * tyscheme) list

(* 型情報は消していいので、これ以降はtexpのままでいいはず。 *)
val check : Syntax.top -> Spec.top -> texp


