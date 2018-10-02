open Lex_1

(* ここで作るっぽい *)
(*
let buf = Lexing.from_channel stdin
let buf = Lexing.from_string "abcabc"

*)

(*
let buf = Lexing.lexbuf_stdin
*)

let buf = Lexing.from_channel stdin


let rec f _ = 
(*
	Scanf.scanf "%c" (fun c -> 
	Printf.printf "%d\n" (Char.code c);
	Printf.printf "%d\n" (Char.code d);
*)
	let d = (main buf) in
	print_int d;
	if d < 0 then () else f ()


let _ = f ()

