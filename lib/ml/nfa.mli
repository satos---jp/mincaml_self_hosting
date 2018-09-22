(*
  ((int * string) list) *  状態と現在の文字列の組
  ((strint option) list)   各変数に対するバインド
*)


type state = 'b 
type nfa = 'a

val step : nfa -> state -> char -> state

