(*
  ((int * string) list) *  状態と現在の文字列の組
  ((strint option) list)   各変数に対するバインド
*)


(*
type state = int
type nfa = int

val step : nfa -> state -> char -> state
*)


type state = (int * (string list)) list
type node = int
type edge = int * int list * int list
type nfa = int * int * int * (char * (node * edge list) list) list

val new_node : nfa -> (nfa * node)

(*
val nfa2str : nfa -> string
*)

val gen_nfa : unit -> nfa

val nfa_add_edge : nfa -> node -> node -> char -> nfa

val step : nfa -> state -> char -> state
val isaccept : nfa -> state -> bool
val isnill : state -> bool
val startstate : nfa -> state

