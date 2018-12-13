(*

Parser.toplevel Lexer.main (Lexing.from_channel stdin)

*)

type parsingact = 
	| Shift of int
	| Reduce of int * int
	| Error

type 'a parsing_data = 
	| Token of 'a                        
	| Datum of int * int * ('a parsing_data) list     

val my_parsing : (int -> bool) * ((int list) list) * ((int * parsingact) list) list -> (unit -> 'a) -> (('a parsing_data) -> int) -> ('a parsing_data)
