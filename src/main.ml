open Syntax
open Source2ast
open Type_checker
open Knorm
open Closure_conv
open Virtual
open Emit_zatsu_x86
open Debug
open Inline
open Common_sube_elim
open Linux_win_diff
open Elim_unused
open Main_option
open Lambda_lift
open Remove_tuple
open Assoc
open Lettuple2dest
open Const_fold

let reduce_funcs = [
	Inline.inliner;
	Assoc.assoc;
	Remove_tuple.remove;
	Const_fold.folder;
	Common_sube_elim.elimer;
	Beta.beter;
	Elim_unused.elim_unused;
]

let reduce_step ast = List.fold_left (fun r -> fun f -> f r) ast reduce_funcs


(*
	Elim_unused.elim_unused (Beta.beter (Common_sube_elim.elimer ast))
	Elim_unused.elim_unused (Beta.beter (Common_sube_elim.elimer ast))
	Elim_unused.elim_unused (Beta.beter (Common_sube_elim.elimer (Inline.inliner ast)))
5回やって、
くらい。
	Elim_unused.elim_unused (Inline.inliner (Beta.beter (Common_sube_elim.elimer ast)))
	Elim_unused.elim_unused (Beta.beter (Common_sube_elim.elimer ast))
let reduce_step ast = 
	Beta.beter (Common_sube_elim.elimer ast)
*)


let reduce ast = 
	if !nooptimization then ast else (
		let rec f i nast = 
			if i < 6 then (
				let tast = reduce_step nast in
				Printf.printf "reduce step %d" i;  print_newline ();
				f (i+1) tast
			) else nast
		in
			f 0 ast
	)

let _ = 
let argc = Array.length Sys.argv in
if argc <= 1 then (
	Printf.printf "Usage: %s filename\n" Sys.argv.(0)
) else (
	let files = (ref [] : string list ref) in
	argparse files;
	files := (
		if !nolib then []
		else if !tortesia then (
			if !asmsin_asmint && (not !in_out_assembler) then ["lib/lib_tortesia.ml"] 
			else ["lib/lib_tortesia.ml"; "lib/lib_sinint.ml"]
		) 
		else ["lib/lib.ml"]
	) @ !files;
	let tast = List.flatten (List.map Source2ast.s2a (!files)) in
	
	print_string (top2str tast);
	
	print_string "parsed"; print_newline ();
	let ast2 = Type_checker.check tast in
	print_string "typed";  print_newline ();
	let kn = Alpha.alpha_conv (Knorm.knorm ast2) [] in
	print_string "k-normalized and alphad";  print_newline ();
	vprint knorm2str kn;
	
	let dkn = Lettuple2dest.lettupledest kn in
	print_string "tuple destructed";  print_newline ();
	vprint knorm2str dkn;
	
	let tkn = reduce dkn in
	print_string "reduced";  print_newline ();
	vprint knorm2str tkn;
	(*
	let ttkn = Lambda_lift.lift tkn in
	print_string "lifted";  print_newline ();
	vprint knorm2str ttkn;
	*)
	let ttkn = tkn in
	let cls = Closure_conv.conv ttkn in
	print_string "closure_converted";  print_newline ();
	vprint clos2str cls;
	let vrt = Virtual.to_virtual cls in
	print_string "virtualized";  print_newline ();
	vprint virt2str vrt;
	let asm = Emit_zatsu_x86.vir2asm vrt in
	vprint (fun x -> x) asm;
	let oc = open_out !output_filename in
	output_string oc asm;
	close_out oc;
(*
nasm lib.s -f elf32 -g -o lib.o; nasm out.s -f elf32 -g -o out.o; gcc -m32 out.o lib.o
*)
	()
)



