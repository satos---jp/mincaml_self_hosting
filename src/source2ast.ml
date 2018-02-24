open Syntax
open Parser
open Debug

let s2a fname = 
	let ic = open_in fname in
	Debug.filename := fname;
	Parser.toplevel Lexer.main (Lexing.from_channel ic)
