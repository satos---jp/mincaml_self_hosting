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
	let ast = Parser.toplevel Lexer.main (Lexing.from_channel ic) in
	if !filename = "pervasive.ml"
		then ast 
		else FDecl(DOpen("lib/ml/pervasive")) ::  ast 

let open2spec fname = 
	let tfn = String.lowercase fname ^ ".mli" in
	let ic = open_in tfn in
	(* Debug.filename := tfn; *)
	Parser.specification_list Lexer.main (Lexing.from_channel ic)


