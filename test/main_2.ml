open Lex_2

let buf = Lexing.from_channel stdin

let rec f _ = 
	let d = (main buf) in
	print_string d;
	print_char 10;
	if d = "END" then () else f ()

let _ = f ()

