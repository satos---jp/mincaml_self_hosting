open Syntax
open Parser
open Debug
open Spec

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
	if fname <> "lib/ml/pervasive.ml" then 
		let tast = FDecl(DOpen("Pervasive")) :: ast in (* TODO(satos) ここmainに回したい *)
		(tast,("Pervasive" :: !spec_open_list))
	else
		(ast,!spec_open_list)
		
(* TODO(satos) ここもmainに回したい *)

let lib_files = ["Pervasive";"List";"String";"Char";"Nfa";"Lexing"]
let open2spec fname = 
	let tfn = String.lowercase fname ^ ".mli" in
	let rfn = (if List.mem fname lib_files then "lib/ml/" else "") ^ tfn in
	let ic = open_in rfn in
	(* Debug.filename := tfn; *)
	Parser.specification_list Lexer.main (Lexing.from_channel ic)


