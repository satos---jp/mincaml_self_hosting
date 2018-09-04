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
open Sys

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

let hasext fn ext = 
	let ls = String.length fn in
	let els = String.length ext in
		ls > els && String.sub fn (ls-els) els = ext

let changext fn ext1 ext2 = 
	let ls = String.length fn in
	let ls1 = String.length ext1 in
	if hasext fn ext1 then
		(String.sub fn 0 (ls-ls1)) ^ ext2
	else
		raise (Failure (Printf.sprintf "%s is not %s file" fn ext1))



let compile file = 
	let tast = Source2ast.src2ast file in
	print_string (top2str tast);
	print_string "parsed"; print_newline ();
	
	let spec = (
		if Sys.file_exists (changext file ".ml" ".mli") then
			Source2ast.open2spec (changext file ".ml" "")
		else []
	) in
	
	let ast2 = Type_checker.check tast spec in
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
	let asm = Emit_zatsu_x86.vir2asm vrt (Type_checker.get_imports ()) (Type_checker.get_exports ()) in
	vprint (fun x -> x) asm;
	asm


let compile_to_sfile fn = 
	let sfn = changext fn ".ml" ".s" in
	let asm = compile fn in
	let oc = open_out sfn in
	output_string oc asm;
	close_out oc;
	sfn

let output_stub files =	
	let ens = List.map (fun fn -> 
		let ts = (
			if hasext fn ".ml" then fn
			else if hasext fn ".s" then changext fn ".s" ".ml"
			else raise (Failure (Printf.sprintf "%s is not .ml nor .s file" fn))
		) in
		ts ^ "main"
	) files 
	in
	let asm = (
		"BITS 32\n" ^ 
		"global _start\n" ^
		"global global_heap\n"
	) ^ (
		String.concat "" (List.map (fun s -> 
			"extern " ^ s ^ "\n"
		) ens)
	) ^ (
		"section .bss\n" ^
		"global_heap:\n" ^
		"\tresb 1000\n" ^ 
		"section .data\n" ^
		"section .text\n" ^
		"_start:\n" ^
		"\tmov esi,global_heap\n"
	) ^ (
		String.concat "" (List.map (fun s -> 
			"\tcall " ^ s ^ "\n"
		) ens)
	) ^ (
		"\tmov eax,1\n" ^
		"\tint 0x80\n"
	)
	in
	let oc = open_out "stub.s" in
	output_string oc asm;
	close_out oc


let exec_command s =
	print_endline s; 
	Sys.command s

let _ = 
	let files = (ref [] : string list ref) in
	argparse files;
	if List.length !files <= 0 then (
		Printf.printf "Usage: %s filename\n" Sys.argv.(0)
	) else (
		if !output_assembler then (
			let _ = List.map compile_to_sfile !files in ()
		) else (
			output_stub !files;
			files := !files @ ["lib/lib.s"; "lib/libio_linux.s"; "stub.s"];
			
			let ofs = List.map (fun fn -> 
				let sfn = (
					if hasext fn ".ml" then compile_to_sfile fn
					else if hasext fn ".s" then fn
					else raise (Failure (Printf.sprintf "%s is not .ml nor .s file" fn))
				) in
				let ofn = changext sfn ".s" ".o" in
				let d = exec_command (Printf.sprintf "nasm %s -f elf32 -g -o %s" sfn ofn) in
				if d = 0 then ofn
				else raise (Failure (Printf.sprintf "nasm for %s failed" sfn))
			) !files
			in
			let fs = String.concat " " ofs in
			let _ = exec_command (Printf.sprintf "gcc -m32 -nostdlib %s -o %s" fs !output_filename) in ()
		)
	)



