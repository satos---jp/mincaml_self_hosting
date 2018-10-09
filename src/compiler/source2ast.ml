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

let stdlib_list = ["lib/ml/list.ml"; "lib/ml/string.ml"; "lib/ml/nfa.ml"; "lib/ml/lexing.ml"] (* ; "lib/ml/parsing.ml"] *)

let src2ast fname = 
	let ic = open_in fname in
	Debug.filename := (basename fname);
	let ast = Parser.toplevel Lexer.main (Lexing.from_channel ic) in
	let rec f v acc = 
		match v with
		| s :: xs -> (
				if !filename = (basename s) then (List.rev acc) @ ast
				else (
					let ls = String.length s in
					f xs (FDecl(DOpen(String.sub s 0 (ls-3))) :: acc)
				)
			)
		| [] -> (List.rev acc) @ ast
	in
		f ("lib/ml/pervasive.ml" :: stdlib_list) []


let open2spec fname = 
	let tfn = String.lowercase fname ^ ".mli" in
	let ic = open_in tfn in
	(* Debug.filename := tfn; *)
	Parser.specification_list Lexer.main (Lexing.from_channel ic)


