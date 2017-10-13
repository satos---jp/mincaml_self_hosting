open Syntax
open Source2ast
open Type_checker
open Knorm
open Closure_conv
open Virtual
open Emit_zatsu_x86
open Debug

(* let _ = Source2ast.s2a "../tes.ml" *)

let _ = 
let argc = Array.length Sys.argv in
if argc <= 1 then (
	Printf.printf "Usage: %s filename\n" Sys.argv.(0)
) else (
	let ast = Source2ast.s2a Sys.argv.(1) in
	Array.set Sys.argv 1 "lib.ml";
	let imps = (Array.sub Sys.argv 1 (argc-1)) in
	let globasts = Array.map Source2ast.s2a imps in
	let tast = match ast with
	| DExpr east -> (
		Array.fold_right (fun gast -> fun gr -> 
				match gast with
				| DDecl xs -> (List.fold_right (fun f -> fun r -> (f r)) xs gr)
				| _ -> raise (Failure "globs is not decl")
			) globasts east 
		)
	| _ -> raise (Failure "inputfile is not value")  in
	(* print_string (expr2str tast); *)
	print_string "parsed"; print_newline ();
	let ttast = 
		(ELetRec("@@main",["@@main_var"],tast,
			(EApp((EVar("@@main"),default_debug_data),[(ETuple([]),default_debug_data)]),default_debug_data)
		),default_debug_data) in
	let ast2 = Type_checker.check ttast in
	print_string "typed";  print_newline ();
	let kn = Knorm.knorm ast2 in
	print_string "k-normalized";  print_newline ();
	let cls = Closure_conv.conv kn in
	(* print_string (clos2str cls); *)
	print_string "closure_converted";  print_newline ();
	let vrt = Virtual.to_virtual cls in
	print_string "virtualized";  print_newline ();
	let asm = Emit_zatsu_x86.vir2asm vrt in
	print_string asm;
	let oc = open_out "out.s" in
	output_string oc asm;
	close_out oc;
(*
nasm lib.s -f elf32 -g -o lib.o; nasm out.s -f elf32 -g -o out.o; gcc -m32 out.o lib.o

17903Ç≈SEGV.
17900 ÇÃ [ebp-368]Ç™Ç‚ÇŒÇ¢ÅB

*)
	()
)



