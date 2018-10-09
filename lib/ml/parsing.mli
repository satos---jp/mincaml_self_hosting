(*

Parser.toplevel Lexer.main (Lexing.from_channel stdin)

*)

type parsingact = 
	| Shift of int
	| Reduce of int
	| Error

val my_parsing : int -> int
