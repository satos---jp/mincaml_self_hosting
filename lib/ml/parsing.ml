(*

Parser.toplevel Lexer.main (Lexing.from_channel stdin)


val my_parsing : data -> (unit -> 'a) -> ('a -> int) -> dataから決まる型
*)

(*
type 'a parsing_data = 
	| Token of 'a                
	| Datum of int * data list 
*)

(* token のやつ *)
(* 規則iによって縮約された *)

type parsingact = 
	| Shift of int
	| Reduce of int
	| Error

let rec split_list v p = 
	if p = 0 then ([],v) else 
	match v with
	| x :: xs -> (
			let (ra,rb) = split_list xs (p-1) in
			(ra @ [x],rb)
		)

let my_parsing (istoplevel,rules,table) lexfun token2id = 
	let get_act st x = (* 状態stからデータxが降ってきた際の挙動 *)
		let qt = List.nth table st in
		let act = List.nth qt (token2id x) in
		act
	in
	let rec steps vs st = 
		match vs with
		| x :: xs -> (
				let act = get_act st x in
				match act with
				| Shift tst -> steps xs tst
				| Reduce d -> Some d
			)
		| [] -> None
	in
	let rec circle vs = 
		let st = 0 in
		let td = steps (List.rev vs) st in
		match td with
		| Some x -> (
				let ls = List.nth rules x in (* 頭ls個を縮約する *)
				let (v,w) = split_list vs ls in
				let fv = Datum(x,v) in
				if istoplevel x then fv
				else circle (fv :: w)
			)
		| None -> circle ((lexfun ()) :: vs)
	in
	circle []



