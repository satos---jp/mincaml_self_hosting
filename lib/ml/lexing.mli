(*
type node = int
type edge = int * int list * int list
type nfa = int * int * int * (char * (node * edge list) list) list
*)

type lexbuf = unit -> char
val my_lexing : lexbuf -> (nfa * (unit -> 'a)) list -> 'a



val from_channel : int -> lexbuf

