(*
type node = int
type edge = int * int list * int list
type nfa = int * int * int * (char * (node * edge list) list) list
*)

type lexbuf = (unit -> char) * (int -> unit)
val my_lexing : lexbuf -> (nfa * ((string list) -> 'a) * int) list -> 'a



val from_channel : int -> lexbuf

