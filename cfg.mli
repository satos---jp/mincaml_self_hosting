open Closure_conv
open Op

(* 関数名 mainかどうか 引数 クロージャ引数 AST グローバル関数名 ヒープ変数名 *)
val cfg_toasms : name -> bool -> (name list) -> cexp -> (string list) -> (string list) -> (op list)

