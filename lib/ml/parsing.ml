(*

Parser.toplevel Lexer.main (Lexing.from_channel stdin)


val my_parsing : data -> (unit -> 'a) -> ('a -> int) -> dataから決まる型
*)


type 'a data = 
	| Token of 'a                (* token のやつ *)
	| Datum of int * int * data list (* データとしてはi番めのやつで、そのうち規則jによって縮約された *)

type parsingact = 
	| Shift of int
	| Reduce of int

let rec split_list v p = 
	if p = 0 then ([],v) else 
	match v with
	| x :: xs -> (
			let (ra,rb) = split_list xs (p-1) in
			(ra @ [x],rb)
		)

let my_parsing (istoplevel,rules,table) lexfun token2id = 
	let get_act st i = (* 状態stからi番めのデータが降ってきた際の挙動 *)
		let qt = List.nth table st in
		let act = List.nth qt i in
		act
	in
	let rec steps vs st = 
		match vs with
		| x :: xs -> (
				let act = get_act st (token2id x) in
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
				let (ls,f) = List.nth rules x in
				let v,w = split_list vs ls in
				if istoplevel x then f v
				else circle ((f v) :: w)
			)
		| None -> circle ((lexfun ()) :: vs)
	in
	circle []



