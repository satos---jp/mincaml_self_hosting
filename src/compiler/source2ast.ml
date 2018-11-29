open Syntax
open Parser
open Debug
open Spec
open Main_option

let basename s = 
	try
		let p = String.rindex s '/' in
		let ls = String.length s in
		String.sub s (p+1) (ls-p-1)
	with
		| Not_found -> s

let src2ast fname = 
	let ic = open_in fname in
	Debug.filename := (basename fname);
	spec_open_list := [];
	let ast = Parser.toplevel Lexer.main (Lexing.from_channel ic) in
	let opens = !spec_open_list in
	spec_open_list := [];
	if fname <> (!path_to_library ^ "/ml/pervasive.ml") then 
		let tast = FDecl(DOpen("Pervasive")) :: ast in (* TODO(satos) ここmainに回したい *)
		(tast,("Pervasive" :: opens))
	else
		(ast,opens)

(* TODO(satos) ここもmainに回したい *)

let lib_files = ["Pervasive";"List";"String";"Char";"Nfa";"Lexing";"Parsing"]
let open2spec fname = 
	let tfn = String.lowercase fname ^ ".mli" in
	let rfn = (if List.mem fname lib_files then !path_to_library ^ "/ml/" else "") ^ tfn in
	let ic = open_in rfn in
	(* Debug.filename := tfn; *)
	spec_open_list := [];
	let ast = Parser.specification_list Lexer.main (Lexing.from_channel ic) in
	let opens = !spec_open_list in
	spec_open_list := [];
	(ast,opens)


