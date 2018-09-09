type nfa
type state
val step : nfa -> state -> char -> stete
val isaccept : nfa -> state -> bool
val isnill : state -> bool
val startstate : nfa -> state

