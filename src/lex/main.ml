open Syntax
open Parser
open Lexer
open Convert

let rec src2ast fname = 
	let ic = open_in fname in
	let ast = Parser.toplevel Lexer.main (Lexing.from_channel ic) in
	ast


let _ = 
	let files = List.tl (Array.to_list Sys.argv) in
	if List.length files <= 0 then (
		Printf.printf "Usage: %s filename\n" Sys.argv.(0)
	) else (
		let ast = src2ast (List.hd files) in 
		let (he,bo) = conv ast in 
		Printf.printf "%s" bo
	)



