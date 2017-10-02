open Syntax
open Source2ast
open Type_checker
open Knorm

(* let _ = Source2ast.s2a "../tes.ml" *)

let _ = 
let argc = Array.length Sys.argv in
if argc <= 1 then (
	Printf.printf "Usage: %s filename\n" Sys.argv.(0)
) else (
	let imps = Array.sub Sys.argv 2 (argc-2) in
	let globasts = Array.map Source2ast.s2a imps in
	let ast = Source2ast.s2a Sys.argv.(1) in
	let tast = match ast with
	| DExpr east -> (
		Array.fold_right (fun gast -> fun gr -> 
				match gast with
				| DDecl xs -> (List.fold_right (fun (na,(ne,de)) -> fun r -> (Syntax.ELet(na,(ne,de),r),de)) xs gr)
				| _ -> raise (Failure "globs is not decl")
			) globasts east 
		)
	| _ -> raise (Failure "inputfile is not value")  in
	let ast2 = Type_checker.check tast in
	let kn = Knorm.knorm ast2 in
	let cld = Closure_conv.conv kn in
	List.iter (fun s -> Printf.printf "%s\n" s)
	!Closure_conv.globals;
	()
)



