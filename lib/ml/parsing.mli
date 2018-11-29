(*

Parser.toplevel Lexer.main (Lexing.from_channel stdin)

*)

type parsingact = 
	| Shift of int
	| Reduce of int * int
	| Error

val my_parsing : (int -> int) * ((int list) list) * ((int * parsingact) list) list -> (unit -> 'a) -> (('a parsing_data) -> int) -> ('a parsing_data)
