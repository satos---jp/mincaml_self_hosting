(*
  ((int * string) list) *  状態と現在の文字列の組
  ((strint option) list)   各変数に対するバインド
*)


(*
type state = int
type nfa = int

val step : nfa -> state -> char -> state
*)

type mem_state =
	| Mem_Some of string
	| Mem_None
	| Mem_End of string

type state = (int * (mem_state list)) list
type node = int
type edge = int * int list * int list
type nfa = int * (int list) * int * (int * (node * edge list) list) list

val new_node : nfa -> (nfa * node)

(*
val nfa2str : nfa -> string
*)

val gen_nfa : unit -> nfa

val nfa_add_edge : nfa -> node -> node -> int -> nfa
val nfa_add_epsilon_edge : nfa -> node -> node -> nfa
val nfa_set_as : nfa -> node -> node -> int -> nfa
val nfa_unset_as : nfa -> node -> node -> int -> nfa

val get_mem_from_state : nfa -> state -> string list
val nfa2start_state : nfa -> int -> state

val step : nfa -> state -> char -> state
val isaccept : nfa -> state -> bool
val isnill : state -> bool


