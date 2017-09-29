open Syntax
open Source2ast
open Type_checker
open Knorm

(* let _ = Source2ast.s2a "../tes.ml" *)

let _ = 
if Array.length Sys.argv <= 1 then (
	Printf.printf "Usage: %s filename\n" Sys.argv.(0)
) else (
	let ast = Source2ast.s2a Sys.argv.(1) in
	let ast2 = Type_checker.check ast in
	let kn = Knorm.knorm ast2 in
	()
)



