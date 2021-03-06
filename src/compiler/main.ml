open Syntax
open Source2ast
open Preprocess
open Type_checker
open Type_expr
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
	(* Common_sube_elim.elimer; TODO(satoS) これやっぱり残念ぽいのでよくする(大テーブルとかがだめ) *)
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



let load_source file = 
	Printf.printf "compile %s\n" file;
	let ast,opens = Source2ast.src2ast file in
	vprint top2str ast;
	ivprint "parsed";
	
	let spec,sopens = (
		if Sys.file_exists (changext file ".ml" ".mli") then
			Source2ast.open2spec (changext file ".ml" "")
		else [],[]
	) in
		(file,ast,spec,(opens @ sopens)) (* TODO(satos) ここガバなのでどうにかしたいですね(そもそもtoplevelの型を変えるとか？)*)

let compile (file,ast,spec,opens) =
	let astp = Preprocess.preprocess ast in
	print_string "preprocessed";  print_newline ();
	vprint top2str astp;
	let ast2 = Type_checker.check file astp spec opens in
	print_string "typed";  print_newline ();
	vprint texp2str ast2;
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
	let asm = Emit_zatsu_x86.vir2asm vrt (Type_checker.get_imports ()) (Type_checker.get_exports ()) (basename file) in
	ivprint asm;
	asm



(* TODO(satos) 設計がつらいので直す *)
let filename_to_sfile fn = 
	let sfn = changext fn ".ml" ".s" in
	let asm = fn |> load_source |> compile in
	let oc = open_out sfn in
	output_string oc asm;
	close_out oc;
	sfn


let ast_spec_to_sfile (fn,ast,spec,opens) = 
	let sfn = changext fn ".ml" ".s" in
	let asm = compile (fn,ast,spec,opens) in
	let oc = open_out sfn in
	output_string oc asm;
	close_out oc;
	sfn


let get_header file = 
	Printf.printf "compile %s\n" file;
	let tast,opens = Source2ast.src2ast file in
	vprint top2str tast;
	ivprint "parsed";
	let astp = Preprocess.preprocess tast in
	let hs = Type_checker.export_header file astp opens in
	print_string "typed";  print_newline ();
	Spec.top2header hs opens

let compile_to_header fn = 
	let hfn = changext fn ".ml" ".mli" in
	(if Sys.file_exists hfn then raise (Failure (Printf.sprintf "%s already exists" hfn)) else ());
	let head = get_header fn in
	let oc = open_out hfn in
	output_string oc head;
	close_out oc;
	hfn

let output_stub files =	
	let ens = List.map (fun fn -> 
		Printf.printf "stub %s\n" fn;
		let ts = (
			if hasext fn ".ml" then fn
			else raise (Failure (Printf.sprintf "%s is not .o file" fn))
		) in
		(basename ts) ^ "main"
	) files 
	in
	let asm = (
		"BITS 32\n" ^ 
		"global _start\n" ^
		"extern heap_init\n"
	) ^ (
		String.concat "" (List.map (fun s -> 
			"extern " ^ s ^ "\n"
		) ens)
	) ^ (
		"section .text\n" ^
		"_start:\n" ^
		"\tcall heap_init\n"
	) ^ (
		(* これ、解決順が重要。 *)
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
			let _ = List.map filename_to_sfile !files in ()
		) else if !output_header then (
			let _ = List.map compile_to_header !files in ()
		) else (
			
			let libp = !path_to_library in
			(* Filelname,(.ml exist,.s exist) *)
			let stdlib_open_list = [
				("Printf",(false,true));
				("Parsing",(true,false));
				("Lexing",(true,false));
				("Nfa",(true,false));
				("List",(true,false));
				("String",(true,true));
				("Char",(false,true));
				("Pervasive",(true,true))
			] in
			
			let asm_files = ref [libp ^ "/asm/malloc.s";libp ^ "/asm/libio_linux.s";"stub.s"] in
			let open2fn s = 
				let bfn = String.lowercase s in
				if List.mem_assoc s stdlib_open_list then (
					let hasml,hass = List.assoc s stdlib_open_list in
					(if hass then
						let afn = libp ^ "/asm/" ^ bfn ^ ".s" in
						if not (List.mem afn !asm_files) then
							asm_files := afn :: !asm_files
						else ()
					else ());
					if hasml then Some(libp ^ "/ml/" ^ bfn ^ ".ml") else None
				) else (
					Some(bfn ^ ".ml")
				)
			in
			
			(* TODO(satos) いったん循環参照checkはommitする *)
			(* これたぶんcheckするようにするとpervasive.ml がやられますね *)
			(* 初手pervasive.ml 解放はやっていいはず。あとはdagになりそう *)
			(* というか別にDAGだったら入力ファイル順番気にしなくていいのでは？？？ *)
			
			(* fn_ast_specs は、上から順に依存関係がない *)
			let fn_ast_specs = 
				let db_check = ref [] in
				let rec add_open na = 
					try
						List.assoc na (!db_check)
					with
						| Not_found -> (
							assert (hasext na ".ml");
							Printf.printf "open %s\n" na;
							let nc = ref 0 in
							let nv = ref [] in
							let (_,_,_,opens) as d = load_source na in
							db_check := (na,(nv,nc,d)) :: !db_check;
							Printf.printf "%s :: %s\n" na (String.concat " @@ " opens);
							let tos = List.fold_left (fun r x -> match x with None -> r | Some d -> d :: r) [] (List.map open2fn opens) in
							List.iter (fun x -> 
								if x = na || na = libp ^ "/ml/pervasive.ml" then () else (
									Printf.printf "Depend from %s to %s\n" na x; 
									let p,_,_ = add_open x in
									p := na :: !p;
									nc := !nc + 1
								)
							) tos;
							
							(if na = libp ^ "/ml/pervasive.ml" then (
								nc := 0
							) else ());
							(nv,nc,d)
						)
				in
				(* TODO(satos) なんかもう手続き型なんだよな... *)
				List.iter (fun x -> let _ = add_open x in ()) !files;
				List.iter (fun (na,(p,q,_)) -> 
					Printf.printf "Depend edge :: %s :: (%s),%d\n" na (String.concat "," !p) !q
				) !db_check;
				let result = ref [] in
				let rec remove_some ds = 
					match ds with
					| [] -> []
					| (na,(p,q,r)) :: xs -> (
							if !q > 0 then (na,(p,q,r)) :: (remove_some xs) 
							else (
								result := (na,r) :: !result;
								List.iter (fun x -> 
									let _,t,_ = List.assoc x !db_check in
									t := !t - 1;
								) !p;
								xs
							)
						)
				in
				let rec loop () = 
					db_check := remove_some !db_check;
					match !db_check with
					| [] -> ()
					| _ -> loop ()
				in
					loop ();
					!result
			in
			
			List.iter (fun (fn,astspec) -> 
				let sfn = ast_spec_to_sfile astspec in
				Printf.printf "make sfile %s\n" sfn;
				asm_files := sfn :: !asm_files
			) fn_ast_specs;
			
			output_stub (List.rev (List.map fst fn_ast_specs));
			let ofs = List.map (fun sfn -> 
				let ofn = changext sfn ".s" ".o" in
				let d = exec_command (Printf.sprintf "nasm %s -f elf32 -g -o %s" sfn ofn) in
				if d = 0 then ofn
				else raise (Failure (Printf.sprintf "nasm for %s failed" sfn))
			) !asm_files
			in
			let fs = String.concat " " ofs in
			let _ = exec_command (Printf.sprintf "gcc -m32 -nostdlib %s %s -o %s" fs (Printf.sprintf "%s/C/malloc_c.o" libp) !output_filename) in ()
		)
	)



