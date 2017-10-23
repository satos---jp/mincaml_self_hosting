open Syntax
open Source2ast
open Type_checker
open Knorm
open Closure_conv
open Virtual
open Emit_zatsu_x86
open Debug
open Emit_zatsu_tortesia
open Inline
open Common_sube_elim
open Linux_win_diff
open Elim_unused

(* let _ = Source2ast.s2a "../tes.ml" *)

let files = ref []
let nolib = ref false
let verbose = ref false
let tortesia = ref false
let vprint f s = 
	if !verbose then (print_string (f s); print_newline ()) else () 


let reduce_step ast =  
	Elim_unused.elim_unused (Inline.inliner (Beta.beter (Common_sube_elim.elimer ast)))
(*
let reduce_step ast = 
	Beta.beter (Common_sube_elim.elimer ast)
*)


let reduce ast = 
	reduce_step (reduce_step (reduce_step (reduce_step (reduce_step ast))))

let _ = 
let argc = Array.length Sys.argv in
if argc <= 1 then (
	Printf.printf "Usage: %s filename\n" Sys.argv.(0)
) else (
	Arg.parse [
		("-nolib",Arg.Set nolib,"stop including lib.ml");
		("-v",Arg.Set verbose,"verbose debug info");
		("-t",Arg.Set tortesia,"compile for tortesia");
		("-w",Arg.Set windows,"compile for windows x86");
	] (fun fn -> files := (!files) @ [fn]) (Printf.sprintf "Usage: %s filename\n" Sys.argv.(0));
	let ast = Source2ast.s2a (List.hd !files) in
	files := if !nolib then (List.tl !files) else ("lib.ml" :: (List.tl !files));
	let globasts = List.map Source2ast.s2a (!files) in
	let tast = match ast with
	| DExpr east -> (
		List.fold_right (fun gast -> fun gr -> 
				match gast with
				| DDecl xs -> (List.fold_right (fun f -> fun r -> (f r)) xs gr)
				| _ -> raise (Failure "globs is not decl")
			) globasts east 
		)
	| _ -> raise (Failure "inputfile is not value")  in
	(* (expr2str tast); *)
	print_string "parsed"; print_newline ();
	let ast2 = Type_checker.check tast in
	print_string "typed";  print_newline ();
	let kn = Alpha.alpha_conv (Knorm.knorm ast2) [] in
	print_string "k-normalized and alphad";  print_newline ();
	vprint knorm2str kn;
	let tkn = reduce kn in
	print_string "reduced";  print_newline ();
	vprint knorm2str tkn;
	let cls = Closure_conv.conv tkn in
	print_string "closure_converted";  print_newline ();
	vprint clos2str cls;
	let vrt = Virtual.to_virtual cls in
	print_string "virtualized";  print_newline ();

	if !tortesia then (
		let asm = Emit_zatsu_tortesia.vir2asm vrt in
		print_string asm
	)
	else (
		let asm = Emit_zatsu_x86.vir2asm vrt in
		vprint (fun x -> x) asm;
		let oc = open_out "out.s" in
		output_string oc asm;
		close_out oc
	);
(*
nasm lib.s -f elf32 -g -o lib.o; nasm out.s -f elf32 -g -o out.o; gcc -m32 out.o lib.o
*)
	()
)



