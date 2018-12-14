(*
type node = int
type edge = int * int list * int list
type nfa = int * int * int * (char * (node * edge list) list) list
*)

open Nfa

type lexbuf = (unit -> char) * (int -> unit)
val my_lexing : lexbuf -> (nfa_compiled * ((string list) -> 'a) * int) list -> 'a

val new_line : lexbuf -> unit


val from_channel : int -> lexbuf

