open Syntax
open Parser

let s2a fname = 
	let ic = open_in fname in
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
			Printf.printf "line %d\n" (!ln);
			let _ = seek_in ic st in
			let rec f np res = 
				if np == gl then res else
					f (np+1) (res ^ Char.escaped (input_char ic))
			in
			let ts = f st "" in
			print_string (ts ^ "\n");
			(Printf.printf "parse error near characters %d-%d\n" 
        (Parsing.symbol_start ())
        (Parsing.symbol_end ()));
			raise (Failure "source2ast failed"))
