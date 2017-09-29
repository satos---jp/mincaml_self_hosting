open Syntax
open Source2ast
open Type_checker
open Knorm

(* let _ = Source2ast.s2a "../tes.ml" *)

let _ = 
if Array.length Sys.argv <= 1 then (
	Printf.printf "Usage: %s filename\n" Sys.argv.(0)
) else (
	let globast = Source2ast.s2a "../raytracer/globals.ml" in
	let ast = Source2ast.s2a Sys.argv.(1) in
	let tast = match globast,ast with
	| DDecl xs,DExpr y -> (List.fold_left (fun r -> fun (na,ne) -> Syntax.ELet(na,ne,r)) y xs)
	| _ -> raise (Failure "globs is not decl or inputfile is not value")  in
	let ast2 = Type_checker.check tast in
	let kn = Knorm.knorm ast2 in
	()
)



