open Syntax
open Parser
open Debug
open Spec

let s2a fname = 
	let ic = open_in fname in
	Debug.filename := fname;
	Parser.toplevel Lexer.main (Lexing.from_channel ic)
	

let open2spec fname = 
	let tfn = String.lowercase fname ^ ".mli" in
	let ic = open_in tfn in
	Debug.filename := tfn;
	Parser.specification_list Lexer.main (Lexing.from_channel ic)

