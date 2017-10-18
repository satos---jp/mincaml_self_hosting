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

(* let _ = Source2ast.s2a "../tes.ml" *)

let files = ref []
let nolib = ref false
let verbose = ref false
let vprint s = 
	if !verbose then (print_string s; print_newline ()) else () 

let reduce_step ast = 
	Common_sube_elim.elimer (Inline.inliner (Beta.beter (Common_sube_elim.elimer ast)))

let reduce ast = 
	reduce_step (reduce_step (reduce_step ast))
(*
	let ta = (Inline.inliner (Beta.beter ast)) in
		vprint (knorm2str ta);
	(Inline.inliner (Beta.beter 
	(Inline.inliner (Beta.beter ta))
	))
*)

(*
	Inline.inliner (Beta.beter ast)
*)

let _ = 
let argc = Array.length Sys.argv in
if argc <= 1 then (
	Printf.printf "Usage: %s filename\n" Sys.argv.(0)
) else (
	Arg.parse [
		("-nolib",Arg.Set nolib,"stop including lib.ml");
		("-v",Arg.Set verbose,"verbose debug info")
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
	(* print_string (expr2str tast); *)
	print_string "parsed"; print_newline ();
	let ast2 = Type_checker.check tast in
	print_string "typed";  print_newline ();
	let kn = Knorm.knorm ast2 in
	print_string "k-normalized";  print_newline ();
	vprint (knorm2str kn);
	let tkn = reduce kn in
	print_string "reduced";  print_newline ();
	vprint (knorm2str tkn);
	let cls = Closure_conv.conv tkn in
	print_string "closure_converted";  print_newline ();
	vprint (clos2str cls);
	let vrt = Virtual.to_virtual cls in
	print_string "virtualized";  print_newline ();

(*
	let asm = Emit_zatsu_tortesia.vir2asm vrt in
	print_string asm;
*)
	let asm = Emit_zatsu_x86.vir2asm vrt in
	vprint asm;
	let oc = open_out "out.s" in
	output_string oc asm;
	close_out oc;

(*
nasm lib.s -f elf32 -g -o lib.o; nasm out.s -f elf32 -g -o out.o; gcc -m32 out.o lib.o
*)
	()
)



