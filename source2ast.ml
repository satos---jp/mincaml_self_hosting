open Syntax
open Parser

let s2a fname = 
	let ic = open_in fname in
	Syntax.filename := fname;
	try
		Parser.toplevel Lexer.main (Lexing.from_channel ic)
	with
		| Failure s -> (
			Printf.printf "%s\n" s;
			let st,gl = !Syntax.err in
			Printf.printf "%d %d\n" st gl;
			let nl = ref 0 in
			let ln = ref 0 in
			let _ = seek_in ic 0 in
			while (!nl) < st do
				let cl = String.length (input_line ic) in
				nl := (!nl) + cl + 1;
				ln := (!ln) + 1
			done;
			let nld = st - (!nl) + 1 in
			Printf.printf "line %d char %d-%d\n" (if nld == 1 then (!ln)+1 else (!ln)) nld (gl - (!nl) + 1);
			let _ = seek_in ic st in
			let rec f np res = 
				if np == gl then res else
					f (np+1) (res ^ Char.escaped (input_char ic))
			in
			let ts = f st "" in
			print_string (ts ^ "\n");
			raise (Failure "source2ast failed"))
